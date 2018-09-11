// hardcoded data
/*
//vector<string> s = {"Whisk", "chicken", "broth", ",", "oyster","sauce",",", "soy","sauce","," , "fish","sauce",",","white", "sugar", ",","and", "brown","sugar", "together", "in","a", "bowl","until","well","blended", "."};
//vector<string> s_aggr = {"Whisk", "chicken", "broth", ",", "oyster","sauce",",", "soy","sauce","," , "fish","sauce",",","white", "sugar", ",","and", "brown","sugar", "together", "in","a", "bowl","until","well","blended", "."};
*/

#include <sstream>
#include <fstream>
#include <iostream>
#include <string>
#include <climits>
#include <map>
#include <set>
#include <vector>
#include <algorithm>

using namespace std;

const int N = 1000;
//int graph[N][N];
//int graph_final[N][N];
//int dist[N][N];
int size_final;

vector<bool> present_row(N, true);
vector<bool> present_col(N, true);

vector<string> s;
vector<string> s_aggr;
vector<string> s_final;
vector<string> POS;
vector<string> POS_aggr;
vector<string> POS_final;
vector<string> lemma;
vector<string> lemma_aggr;
vector<string> lemma_final;
vector<string> roots;
//vector<int> gov;
//vector<int> dep;
vector<int> beginnings;
vector<int> root_ids;
vector<int> sentence_id;
vector<int> sentence_id_final;
vector<int> is_food;
vector<int> is_obj;
vector<int> is_color;
vector<int> is_disallowed;
//vector<int> is_food_dupl;
vector<int> is_food_final;
vector<int> word_chunks;
vector< vector<int> > id_mapping_aggr;
vector< vector<int> > id_mapping_final;

vector<string> allowed_endings = {
    "piece", "pieces",
    "slice", "slices",
    "flake", "flakes",
    "chip", "chips",
    "crumb", "crumbs",
    "bit", "bits",
    "shaving", "shavings",
    "chunk", "chunks",
    "sliver", "slivers",
    "strip", "strips"
    "grain", "grains",
    "extract",
    "topping", "toppings",
    "nugget", "nuggets",
    "mixture", "mixtures",
    "brew", "concoction",
    "blend", "mix",
    "combination", "seasoning",
    "dressing", " filling",
    "flavoring", "flavouring",
    "topping", "toppings"
    "strip", "strips",
    "chop", "chops"
    };

vector<string> disallowed_endings = {
    "platter", "dish",
    "maker", "pan",
    "paper"
};

int ids_ctr = 0;
vector<int> ids;

map< int , bool> food_to_tag;
map<string, bool> aux_verbs;
map<string, bool> modifier_aux;
map<int, string> rez_modifiers1;



string g_to_lower(string s)
{
    string ret = s;

    for(unsigned int i = 0; i < ret.length(); i++)
    {
        if(ret[i] >= 'A' && ret[i] <= 'Z')
            ret[i] ^= 32;
    }
    return(ret);
}

template <typename T>
void print_vector(vector<T> &a, int w = 0)
{
    cout << "Size: " << a.size() << endl;
    for(int i = 0; i < a.size(); i++)
    {
        cout.width(w);
        cout << a[i] << " ";
    }
    cout << endl;
}

template <typename T>
void print_vector_newline(vector<T> &a)
{
    //cout << "Size: " << a.size() << endl;
    for(int i = 0; i < a.size(); i++)
        cout << a[i] << endl;
    cout << endl;
}

// parse tree data
int n_trees;

class Tree
{
    public:
        static int curr_global_idx;
        int sent_id;
        int n;
        // adjacency matrix
        vector< vector<int> > mat;
        // local tree node (for tokens only) index to global token index
        map<int, int> treeidx_to_globalidx;
        map<int, int> globalidx_to_treeidx;
        // local tree node index to tree node label
        map<int, string> idx_to_label;
        vector<int> node_depths;


        Tree(string fname, int t_n, int s_id) : n(t_n), sent_id(s_id)
        {
            mat.assign(t_n, vector<int>());
            node_depths.assign(t_n, -1);

            fstream inA;
            fstream inB;
            fstream inC;
            fstream inD;

            inA.open((fname + "A.txt"), fstream::in);
            inB.open((fname + "B.txt"), fstream::in);
            inC.open((fname + "C.txt"), fstream::in);
            inD.open((fname + "D.txt"), fstream::in);

            int t1, t2;
            string s1, s2;

            //cout << "------------ " << t_n << endl;
            while(inB >> t1)
            {
                inD >> t2;
                t1--;
                t2--;

                getline(inA, s1);
                getline(inC, s2);

                mat[t1].push_back(t2);
                mat[t2].push_back(t1);


                if(idx_to_label.count(t1) == 0)
                {
                    idx_to_label[t1] = s1;
                }
                if(idx_to_label.count(t2) == 0)
                {
                    idx_to_label[t2] = s2;
                }

                //cout << s1 << " " << t1 << " | " << s2 << " " << t2 << endl;
            }

            inA.close();
            inB.close();
            inC.close();
            inD.close();

            for(int i = 0; i < n; i++)
            {
                sort(mat[i].begin(), mat[i].end());
            }
        }

        void init()
        {
            vector<bool> visited(n, false);
            dfs0(0, 0, visited);


        }
        void print()
        {
            //print_vector(node_depths);
            cout << "----------------" << endl;
            map<int, int>::iterator it = treeidx_to_globalidx.begin();
            for(;it != treeidx_to_globalidx.end(); it++)
            {
                cout << it->first << " " << it->second << " " << s[it->second] << endl;
                //cout << node_depths[it->first] << endl << endl;
            }

            cout << endl;
            it = globalidx_to_treeidx.begin();
            for(;it != globalidx_to_treeidx.end(); it++)
            {
                cout << it->first << " " << it->second << " " << s[it->first] << endl;
                //cout << node_depths[it->first] << endl << endl;
            }

            cout << " ------" << endl << endl;
//            for(int i = 0; i < n; i++)
//            {
//                cout << idx_to_label[i] << "" << endl;
//
//            }
        }
        void find_base_modifiers(int global_id, string fname, int chunk_id)
        {
            int tree_id = globalidx_to_treeidx[global_id];
            //cout << "START " << s[global_id] << endl;

//            this->print();
            //cout << tree_id << " " << global_id << endl;
//            cout << idx_to_label[tree_id] << " " << s[global_id] << endl;
//            cout << idx_to_label[0] << endl << endl;

            string ret2;
            int ret3;
            vector<bool> visited(n, false);
            bool found = false;


            dfs1(tree_id, tree_id, visited, found, ret2, ret3);
            //this->print();

            if(!found)
            {
//                cout << "HELLO " << sent_id-1 << endl;
//                for(int p = 0; p < roots.size(); p++)
//                    cout << roots[p] << " ";
//                cout << endl;
//                ret2 = roots[sent_id-1];
//                ret3 = root_ids[sent_id - 1];
                map<int, int>::iterator it = treeidx_to_globalidx.begin();
                //ret2 = it->first;
                //ret3 = it->second;
                ret2 = s[it->second];
                ret3 = it->second;
//                cout << it-> first << " " << s[it->second] << endl;

            }

            ofstream out;
            out.open("food_modifiers/"+fname, ios_base::app);


            out << s_final[chunk_id] << endl;
            out << ret2 << " [" << ret3 << "]" << endl << endl;

            //cout << "------" << endl;


            out.close();
            //cout << "END" << endl << endl;
        }
        ~Tree()
        {


        }


    private:
        void dfs0(int curr, int depth, vector<bool> &visited)
        {
            if(visited[curr])
                return;

            visited[curr] = true;
            node_depths[curr] = depth;


            int t_count = 0;
            for(int i = 0; i < mat[curr].size(); i++)
            {
                int next = mat[curr][i];
                if(!visited[next])
                {
                    t_count++;
                    dfs0(next, depth+1, visited);
                }
            }
            if(t_count == 0)
            {
//                if(curr == 22)
//                    cout << "FOUND " << idx_to_label[curr] << endl;

                if(treeidx_to_globalidx.count(curr) > 0 || globalidx_to_treeidx.count(curr_global_idx) > 0)
                    cout << "Duplicate tree index to global index mapping!" << endl;

                //cout << "TAGGING " << idx_to_label[curr] << " TO " << curr << " AND " << curr_global_idx<< endl;
                treeidx_to_globalidx[curr] = curr_global_idx;
                globalidx_to_treeidx[curr_global_idx] = curr;


                curr_global_idx++;
            }
        }

        void dfs1(int base, int curr, vector<bool> &visited, bool &found, string &ret2, int &ret3)
        {

           if(visited[curr] || found)
                return;

            visited[curr] = true;

            int t_count = 0;
            for(int i = mat[curr].size()-1; i >= 0; i--)
            {
                int next = mat[curr][i];

                if(!visited[next] && node_depths[next] <= node_depths[base] + 1 && next < base)
                {
                    t_count++;
                    dfs1(base, next, visited, found, ret2, ret3);
                }
            }
            if(t_count == 0 && treeidx_to_globalidx.count(curr) > 0)
            {
                if(treeidx_to_globalidx.count(curr) == 0)
                {

                    //cout << treeidx_to_globalidx[curr] << endl;

                    return;
                }

                int t_global = treeidx_to_globalidx[curr];
                //cout << "\t" << idx_to_label[curr] << endl;
                if(POS[t_global].find("VV0") != string::npos || POS[t_global].find("VVI") != string::npos)
                {
                    //cout << "\tMODIFIER: " << s[t_global] << endl;
                    ret2 = s[t_global];
                    ret3 = t_global;
                    //cout << ret2 << " " << ret3 << endl;
//                    if(ret3 == 0)
//                    {
//                        this->print();
//
//                    }

                    found = true;
                    return;
                }
//                else if(curr == 0)
//                {
//                    cout << "HELLO" << endl;
//                    //cout << "\tMODIFIER: ROOT" << endl;
//                    //cout << sent_id-1 << endl;
//                    ret2 = roots[sent_id-1];
//                    ret3 = root_ids[sent_id - 1];
//                    found =  true;
//                    return;
//                }

            }
        }
};

int Tree::curr_global_idx = 0;

vector<Tree> trees;
//vector<int> tree_sizes;
//vector< map<int, string> > id_to_token;
//vector< map<int, int> > treeid_to_globalid;

//add false negative verbs such as Cook, Place, Season, Marinate...

//try using a lemma if the vector expands significantly


//void print_graph()
//{
//
//    for(int i = 0; i < size_final; i++)
//    {
//        for(int j = 0; j < size_final; j++)
//            cout << graph_final[i][j] << " ";
//        cout << endl;
//    }
//    cout << endl;
//}

//void print_graph_file(string fname)
//{
//    ofstream out("graphs/" + fname);
//
//    for(int i = 0; i < size_final; i++)
//    {
//        for(int j = 0; j < size_final; j++)
//            out << graph_final[i][j] << " ";
//        out << endl;
//    }
//    out << endl;
//
//    out.close();
//}

//void print_graph_indexed()
//{
//    cout.width(3);
//    cout << "x.";
//    for(int i = 0; i < size_final; i++)
//    {
//        cout.width(3);
//        cout << i;
//    }
//    cout << endl;
//
//    for(int i = 0; i < size_final; i++)
//    {
//        cout.width(3);
//        cout << i << ".";
//        for(int j = 0; j < size_final; j++)
//        {
//
//            cout.width(3);
//            cout << graph_final[i][j];
//        }
//        cout << endl;
//    }
//    cout << endl;
//}


//void print_dist()
//{
//    cout.width(3);
//    cout << "x.";
//    for(int i = 0; i < size_final; i++)
//    {
//        cout.width(3);
//        cout << i;
//    }
//    cout << endl;
//
//    for(int i = 0; i < size_final; i++)
//    {
//        cout.width(3);
//        cout << i << ".";
//        for(int j = 0; j < size_final; j++)
//        {
//
//            cout.width(3);
//            cout << dist[i][j];
//        }
//        cout << endl;
//    }
//    cout << endl;
//}

//void print_dist_file(string fname)
//{
//    ofstream out("graphs/" + fname);
//
//    for(int i = 0; i < size_final; i++)
//    {
//        for(int j = 0; j < size_final; j++)
//            out << dist[i][j] << " ";
//        out << endl;
//    }
//    out << endl;
//
//    out.close();
//}


template <typename T>
void print_vector_newline(vector<T> &a, string fname)
{
    //cout << "Size: " << a.size() << endl;

    ofstream out("entities/" + fname);
    for(unsigned int i = 0; i < a.size(); i++)
        out << a[i] << endl;
    out << endl;
    out.close();
}


template <typename T>
void print_vector_indexed_newline(vector<T> &a, int offset)
{
    //cout << "Size: " << a.size() << endl;
    for(int i = 0; i < a.size(); i++)
        cout << i+offset << ". " << a[i] << endl;
    cout << endl;
}

template <typename T>
void print_vector_indexed_newline(vector<T> &a, int offset, string fname)
{
    //cout << "Size: " << a.size() << endl

    ofstream out("entities/" + fname);
    for(int i = 0; i < a.size(); i++)
        out << i+offset << ". " << a[i] << endl;
    out << endl;

    out.close();
}

void print_full(string fname)
{


    //cout << "Size: " << a.size() << endl;

    ofstream out("entities/" + fname);
    for(unsigned int i = 0; i < s_final.size(); i++)
        out << s_final[i] << "( " << POS_final[i] << " )" << " - " << is_food_final[i] << endl;
    out << endl;
    out.close();
}

void get_input()
{
    int a;
    string vlez;
    ifstream in("data/tokens.txt");
    while(getline(in, vlez))
    {
        s.push_back(vlez);
        s_aggr.push_back(vlez);
    }
    in.close();

    in.open("data/lemmas.txt");
    while(getline(in, vlez))
    {
        lemma.push_back(vlez);
        lemma_aggr.push_back(vlez);
    }
    in.close();

    in.open("data/POS.txt");
    while(getline(in, vlez))
    {
        POS.push_back(vlez);
        POS_aggr.push_back(vlez);
    }
    in.close();


    in.open("data/roots.txt");
    while(getline(in, vlez))
    {
        roots.push_back(vlez);
    }
    in.close();

    in.open("data/is_food.txt");
    while(in >> a)
    {
        is_food.push_back(a);
    }
    in.close();

    in.open("data/is_color.txt");
    while(in >> a)
    {
        is_color.push_back(a);
    }
    in.close();

    in.open("data/is_disallowed.txt");
    while(in >> a)
    {
        is_disallowed.push_back(a);
    }
    in.close();

    in.open("data/root_ids.txt");
    while(in >> a)
    {
        root_ids.push_back(a);
    }
    in.close();

    in.open("data/is_object.txt");
    while(in >> a)
    {
        is_obj.push_back(a);
    }
    in.close();


    in.open("data/sentence_ids.txt");
    while(in >> a)
    {
        sentence_id.push_back(a);
    }
    in.close();


    vector<int> sizes;
    in.open("data/trees/count.txt");
    in >> n_trees;
    while(in >> a)
    {
        sizes.push_back(a);
    }
    in.close();


    for(int i = 0; i < n_trees; i++)
    {
        string fname = to_string(i+1);

        string path = "data/trees/" + fname;
        trees.push_back(Tree(path, sizes[i], i+1));
        trees[i].init();
       // trees[i].print();
    }
}

bool check_POS_left(int i)
{
    if(!present_row[i] || is_obj[i])
        return false;

    // try J instead of JJ

    if(POS[i].find("JJ") != string::npos ||  POS[i].find("NP") != string::npos || POS[i].find("Z99") != string::npos || food_to_tag.count(i) > 0 || POS[i].find("NN") != string::npos || POS[i].find("GE") == 0)
    {
//        cout << "TRUE:" << endl;
//        cout << "\t" << s[i] << " " << POS[i] << endl << endl;
        return(true);
    }

    return(false);


}

bool check_POS_right(int i)
{
    if(!present_row[i])// || is_disallowed[i])
        return false;

    // try J instead of JJ

    // try this instead
    //if(POS[i].find("JJ") != string::npos ||  POS[i].find("NP") != string::npos || POS[i].find("Z99") != string::npos || food_to_tag.count(i) > 0 || POS[i].find("NN") != string::npos )
    if(POS[i].find("JJ") != string::npos || POS[i].find("Z99") != string::npos || food_to_tag.count(i) > 0 || is_color[i] == 1 || POS[i].find("NN") != string::npos|| POS[i].find("NP") != string::npos || find(allowed_endings.begin(), allowed_endings.end(), s[i]) != allowed_endings.end() || POS[i].find("GE") == 0)
    {
//        cout << "TRUE:" << endl;
//        cout << "\t" << s[i] << " " << POS[i] << endl << endl;
        return(true);
    }

    return(false);


}


void join_chunks()
{
    vector<int> converted;
    for(unsigned int i = 0; i < s.size(); i++)
    {
        converted.clear();
        if(food_to_tag.count(i) > 0 && ids[i] == -1)
        {
//            cout << "AT: " << s[i] << endl;
            ids_ctr++;
            ids[i] = ids_ctr;
            converted.push_back(i);
            for(int j = i; j >= 0; j--)
            {
                if(check_POS_left(j))
                {
                   // cout << "\t L " << s[j] << endl;
                    ids[j] = ids_ctr;
                    converted.push_back(j);
                }
                else
                    break;
            }

            for(unsigned int j = i; j < s.size(); j++)
            {
                if(check_POS_right(j))
                {
                    //cout << "\t R" << s[j] << endl;
                    ids[j] = ids_ctr;
                    converted.push_back(j);
                }
                else
                    break;
            }

            if(converted.size() == 0)
                continue;

            int last = converted[converted.size()-1];

            bool to_delete = true;
            for(unsigned int l = 0; l < allowed_endings.size(); l++)
            {

                if(g_to_lower(s[last]) == allowed_endings[l])
                {
                    cout << "ALLOWED " << s[last] << " BECAUSE " << allowed_endings[l] << endl;
                    to_delete = false;
                    break;
                }

            }

            if(!to_delete)
                continue;
            //if(POS[last].find("NN") != string::npos && food_to_tag.count(s[last]) == 0)
//            cout << s[last] << " with " << is_obj[last] << endl;
//            ofstream out;
//            out.open("TESTING/FPNP.txt", ios_base::app);
            //cout << s[last] << " " << is_obj[last] << endl;
            //if(POS[last].find("NN") != string::npos && is_obj[last] == 1 && is_food[last] == 0 || is_disallowed[last])
//            cout << s[last] <<  " " << (find(disallowed_endings.begin(), disallowed_endings.end(), s[last]) != disallowed_endings.end()) << endl;
//            cout << disallowed_endings[1] << endl;
            if((POS[last].find("NN") != string::npos && is_obj[last] == 1)  || is_disallowed[last] || (find(disallowed_endings.begin(), disallowed_endings.end(), s[last]) != disallowed_endings.end()))
            {
                cout << "Reverting.." << endl;
                cout << POS[last] << " - " << s[last] << endl;
                for(unsigned int p = 0; p < converted.size(); p++)
                {
                    ids[converted[p]] = -1;
                }
            }
//            out.close();

        }
    }

}

void make_chunks()
{
    /*// use openNLP chunk data?
    for(int i = 0; i < dep.size(); i++)
    {
        graph[dep[i]-1][gov[i]-1] = 1;
        graph[gov[i]-1][dep[i]-1] = 1;
    }*/


    for(unsigned int i = 0; i < s.size(); i++)
    {
        id_mapping_aggr[i].push_back(i);
    }

    for(unsigned int i = 0; i < s.size()-1; i++)
    {
        //cout << "Checking " << s[i] << endl;
        //cout  << "------" << i << endl;
        if(ids[i] == -1)
        {
            continue;
        }

    //cout << "start " << s[i] << endl;
        for(unsigned int j = i+1; j < s.size(); j++)
        {
            if(ids[i] != ids[j])
            {
                i = j-1;
                break;
            }
            //cout << "\t taken " << s[j] << endl;
            /*
            for(int k = 0; k < s.size(); k++)
            {
                if(!present_row[k])
                    continue;
                graph[i][k] |= graph[j][k];
            }

            for(int k = 0; k < s.size(); k++)
            {
                if(!present_col[k])
                    continue;
                graph[k][i] |= graph[k][j];
            }
            */
            //cout << "adding " << s[j] << endl;
            id_mapping_aggr[i].push_back(j);
            s_aggr[i] += " " + s_aggr[j];
            lemma_aggr[i] += " " + lemma_aggr[j];
            POS_aggr[i] += " " + POS_aggr[j];
            present_row[j] = false;
            present_col[j] = false;
        }

    }

    int koj = 0, koj2 = 0;
    for(unsigned int i = 0; i < s.size(); i++)
    {
        if(!present_row[i])
            continue;

        sentence_id_final.push_back(sentence_id[i]);
        s_final.push_back(s_aggr[i]);
        lemma_final.push_back(lemma_aggr[i]);
        POS_final.push_back(POS_aggr[i]);
        id_mapping_final.push_back(id_mapping_aggr[i]);

        /*koj2 = 0;
        for(int j = 0; j < s.size(); j++)
        {
            if(!present_col[j])
                continue;

            graph_final[koj][koj2] = graph[i][j];
            koj2++;
        }*/
        koj++;
    }
    size_final = koj;


    /*for(int i = 0; i < size_final; i++)
    {
        graph_final[i][i] = 0;
    }*/
}

void init()
{
    aux_verbs["be"] = true;
    aux_verbs["can"] = true;
    aux_verbs["do"] = true;
    aux_verbs["have"] = true;
    aux_verbs["may"] = true;
    aux_verbs["might"] = true;
    aux_verbs["must"] = true;
    aux_verbs["need"] = true;
    aux_verbs["shall"] = true;
    aux_verbs["will"] = true;

    modifier_aux["become"] = true;
    modifier_aux["get"] = true;
    //modifier_aux["turn"] = true;


    id_mapping_aggr.assign(s.size(), vector<int>());
//    N = s.size(); // ?
//    for(int i = 0;i < N+1; i++)
//    {
//        for(int j = 0; j < N; j++)
//            graph[i][j] = 0;
//    }


    // preprocessing - start

    for(unsigned int i = 0; i < s.size(); i++)
    {
        ids.push_back(-1);
    }

//    for(int i = 0; i < beginnings.size()-1; i++)
//    {
//
//        for(int j = 0; j < beginnings[i+1] - beginnings[i]; j++)
//        {
//            word_chunks.push_back(beginnings[i]);
//            //is_food_dupl.push_back(is_food[i]);
//        }
//    }




//    for(int j = 0; j < s.size() - beginnings[beginnings.size()-1] + 1; j++)
//    {
//        word_chunks.push_back(beginnings[beginnings.size()-1]);
//        //is_food_dupl.push_back(is_food[beginnings.size()-1]);
//    }

    for(unsigned int i = 0; i < s.size(); i++)
    {
    	//TODO: FIX so it doesn't split compund nouns (NN + NN) ?
        if(is_food[i] && POS[i].find("N") == 0)
        {
            //cout << s[i] << endl;
            food_to_tag[i] = true;
        }
    }
}

//void change_sign()
//{
//    for(int i = 0; i < size_final; i++)
//    {
//        for(int j = 0; j < size_final; j++)
//            graph_final[i][j] *= -1;
//    }
//}

//void FW()
//{
//    for(int i = 0; i < size_final; i++)
//    {
//        for(int j = 0; j < size_final; j++)
//        {
//            if(graph_final[i][j] == 0)
//                dist[i][j] = INT_MAX/2;
//            else
//                dist[i][j] = graph_final[i][j];
//        }
//        dist[i][i] = 0;
//    }
//
//    change_sign();
//    int n = size_final, d;
//    for(int k = 0; k < n; k++)
//    {
//        for(int i = 0; i < n; i++)
//        {
//            for(int j = 0; j < n; j++)
//            {
//                d = dist[i][k] + dist[k][j];
//                if(d < dist[i][j])
//                {
//                    dist[i][j] = d;
//
//                }
//            }
//        }
//    }
//    change_sign();
//}


//void make_pairs(int d_max)
//{
//    int n = size_final;
//
//
//    for(int k = 1; k <= d_max; k++)
//    {
//
//        cout << "-------------Distance " << k << "-------------" << endl;
//        for(int i = 0; i < n; i++)
//        {
//            if(!is_food_final[i])
//                continue;
//
//            cout << s_final[i] << endl;
//
//            /*for(int j = 0; j < n; j++)
//            {
//                if(dist[i][j] == k)
//                    cout << "    " << s_final[j] << endl;
//            }*/
//        }
//        cout << endl;
//    }
//}
//
//void make_pairs_file(int d_max, string fname)
//{
//    int n = size_final;
//
//    ofstream out("outputs/" + fname);
//
//    for(int k = 1; k <= d_max; k++)
//    {
//
//        out << "-------------Distance " << k << "-------------" << endl;
//        for(int i = 0; i < n; i++)
//        {
//            if(!is_food_final[i])
//                continue;
//
//            out << s_final[i] << endl;
//
//           /* for(int j = 0; j < n; j++)
//            {
//                if(dist[i][j] == k)
//                    out << "    " << s_final[j] << endl;
//            }*/
//        }
//        out << endl;
//    }
//    out.close();
//}


void print_foods(string fname)
{
    int n = size_final;

    ofstream out("food_chunks/" + fname);
    for(int i = 0; i < n; i++)
    {
        if(!is_food_final[i])
            continue;

        //cout << s_final[i] << endl;
        out << s_final[i] << endl;
    }
    out.close();
}

//void find_modifiers(string fname)
//{
//    int koj;
//    int n = size_final;
//
////
////    for(int i = 0; i < n; i++)
////    {
////
////        if(lemma_final[i].find("until") != string::npos)
////            cout << "AT: " << i << endl;
////    }
//
//
//    ofstream out("modifiers/" + fname);
//
//    for(int i = 0; i < n; i++)
//    {
//        if(!is_food_final[i])
//            continue;
//
//        out << s_final[i] << endl;
//
//
//        // ommit modals and other types
//        for(int j = i-1; j >= 0 && sentence_id_final[i] == sentence_id_final[j]; j--)
//        {
//            if(POS_final[j].find("VV0") != string::npos || POS_final[j].find("VVI") != string::npos)
//            {
//                if(POS_final[j+1].find("VVG") != string::npos)
//                    koj = j+1;
//                else if(POS_final[j+1].find("TO") != string::npos)
//                    koj = j + 2;
//                else
//                    koj = j;
//
//                out << "    " << "A - " << koj << " - " << s_final[koj] << endl;
//            }
//        }
//
//        //break if imperative
//        //for(int j = i+1; j < n && sentence_id_final[i] == sentence_id_final[j]; j++)
//        for(int j = i+1; j < n && sentence_id_final[i] == sentence_id_final[j]; j++)
//        {
//            if(is_food_final[j] == 1 || POS_final[j].find("VV0") != string::npos || POS_final[j].find("VVI") != string::npos)
//            {
//                break;
//            }
//
//            if(lemma_final[j].find("until") != string::npos)
//            {
//                int idx=-1;
//                koj = -1;
//                for(int k = j+1; k < size_final && sentence_id_final[j] == sentence_id_final[k]; k++)
//                {
//                    if(POS_final[k].find("VBZ") != string::npos)
//                    {
//                        idx = k;
//                        break;
//                    }
//                }
//                if(idx == -1)
//                {
//                    idx = j;
//                }
//
//                for(int k = idx+1; k < size_final && sentence_id_final[j] == sentence_id_final[k]; k++)
//                {
//
//                    if((POS_final[k][0] == 'V') || (POS_final[k][0] == 'J') || POS_final[k].find("RR") != string::npos)
//                    {
//                        koj = k;
//                    }
//                    else
//                        break;
//
//
//                }
//                if(koj > -1)
//                    out << "    " << "P - " << koj << " - " << s_final[koj] << endl;
//            }
//        }
//
//        //2
//        //break if imperative
//        //for(int j = i+1; j < n && sentence_id_final[i] == sentence_id_final[j]; j++)
//        for(int j = i+1; j < n && sentence_id_final[i] == sentence_id_final[j]; j++)
//        {
//            if(is_food_final[j] == 1 || POS_final[j].find("VV0") != string::npos || POS_final[j].find("VVI") != string::npos)
//            {
//                break;
//            }
//
//            if(POS_final[j].find("VBZ") != string::npos)
//            {
//                int idx=-1;
//                koj = -1;
//
//                for(int k = j; k < size_final && sentence_id_final[j] == sentence_id_final[k]; k++)
//                {
//
//                    if((POS_final[k][0] == 'V') || (POS_final[k][0] == 'J') || POS_final[k].find("RR") != string::npos)
//                    {
//                        koj = k;
//                    }
//                    else
//                        break;
//
//
//                }
//                if(koj > -1)
//                    out << "    " << "P - " << koj << " - " << s_final[koj] << endl;
//            }
//        }
//    }
//    out << endl;
//    out.close();
//}


vector<string> fixed_verbs = {
"cook",
"broil",
"season"
};



void fix_beginnings()
{
    int prev = -1;
    for(int i = 0; i < s.size(); i++)
    {
        if(find(fixed_verbs.begin(), fixed_verbs.end(), s[i]) != fixed_verbs.end())
        {
            POS[i] = "VVI";
            is_food[i] = 0;
        }

        if(sentence_id[i] != prev)
        {
            if(POS[i].find("JJ") != string::npos || POS[i].find("NN") != string::npos)
            {
                //cout << s[i] << " " << POS[i] << endl;
                POS[i] = "VV0";
                is_food[i] = 0;
                is_obj[i] = 0;
            }


        }

        prev = sentence_id[i];
    }

}

void find_foods()
{
    int brojac;
    bool found = false;
    string zbor;
    stringstream ss;
    for(int i = 0; i < size_final; i++)
    {
        found = false;
        ss.clear();
        ss << s_final[i];
        brojac = 0;
        //cout << s_final[i] << " with " << id_mapping_final[i].size() << endl;
        while(ss >> zbor)
        {
            //cout << food_to_tag.count(zbor) << " ---------" << ids[id_mapping_final[i][brojac]] << endl;
            if(food_to_tag.count(id_mapping_final[i][brojac]) > 0 && ids[id_mapping_final[i][brojac]] > -1)
            {
                found = true;
                break;
            }
            brojac++;
        }

        if(found)
        {
            is_food_final.push_back(1);
        }
        else
            is_food_final.push_back(0);
    }

}

void find_modifiers(string fname)
{
    for(int i = 0; i < size_final; i++)
    {
        if(!is_food_final[i])
            continue;

        int t = id_mapping_final[i].size() - 1;
        int token_id = id_mapping_final[i][t];
        int sent = sentence_id[token_id] - 1;

//        cout << "Size: " << trees.size() << endl;
//        cout << s[token_id] << " " << token_id << " in sentence " << sent << endl << endl;

        trees[sent].find_base_modifiers(token_id, fname, i);

    }
}


int main(int argc,char* argv[])
{
    string path;
    if(argc < 2)
    {
        cout << "No output file name given! Proceeding with test.txt!" << endl;
//        get_input();
//        init();
//        join_chunks();
//        make_chunks();
//        find_foods();
//        find_modifiers("test.txt");
//        print_foods("test.txt");

        path = "test.txt";

    }
    else
    {
        path = argv[1];

    }
//    get_input();
//    init();
//    //cout << "HELLO" << endl;
//    join_chunks();
//    make_chunks();
//    /// FLOYD WARSHALL FUNCTION
//    //FW();
//    ///
//    find_foods();
//    find_modifiers(argv[1]);


        get_input();
        fix_beginnings();
        init();
        join_chunks();
        make_chunks();
        find_foods();
        find_modifiers(path);

        //print_vector_newline(s_final);
//        print_vector_newline(is_food_final);


    //output
    //print_vector_newline(s_final, path);
    print_foods(path);
//    print_vector_newline(s_final);
//    for(int i = 0; i < ids.size(); i++)
//    {
//
//        cout << ids[i] << " " << s[i] << endl;
//    }

    return(0);
}
