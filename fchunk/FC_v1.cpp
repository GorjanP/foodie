// hardcoded data
/*
//vector<string> s = {"Whisk", "chicken", "broth", ",", "oyster","sauce",",", "soy","sauce","," , "fish","sauce",",","white", "sugar", ",","and", "brown","sugar", "together", "in","a", "bowl","until","well","blended", "."};
//vector<string> s_aggr = {"Whisk", "chicken", "broth", ",", "oyster","sauce",",", "soy","sauce","," , "fish","sauce",",","white", "sugar", ",","and", "brown","sugar", "together", "in","a", "bowl","until","well","blended", "."};
*/

#include <sstream>>
#include <fstream>
#include <iostream>
#include <climits>
#include <map>
#include <set>
#include <vector>
#include <algorithm>

using namespace std;

const int N = 1000;
int graph[N][N];
int graph_final[N][N];
int dist[N][N];
int size_final;

vector<bool> present_row(N, true);
vector<bool> present_col(N, true);

vector<string> s;
vector<string> s_aggr;
vector<string> s_final;
vector<string> POS;
vector<string> POS_aggr;
vector<string> POS_final;
vector<int> gov;
vector<int> dep;
vector<int> beginnings;
vector<int> is_food;
//vector<int> is_food_dupl;
vector<int> is_food_final;
vector<int> word_chunks;

map<string, bool> food_to_tag;

void print_graph()
{

    for(int i = 0; i < size_final; i++)
    {
        for(int j = 0; j < size_final; j++)
            cout << graph_final[i][j] << " ";
        cout << endl;
    }
    cout << endl;
}
void print_graph_file(string fname)
{
    ofstream out("graphs/" + fname);

    for(int i = 0; i < size_final; i++)
    {
        for(int j = 0; j < size_final; j++)
            out << graph_final[i][j] << " ";
        out << endl;
    }
    out << endl;

    out.close();
}

void print_graph_indexed()
{
    cout.width(3);
    cout << "x.";
    for(int i = 0; i < size_final; i++)
    {
        cout.width(3);
        cout << i;
    }
    cout << endl;

    for(int i = 0; i < size_final; i++)
    {
        cout.width(3);
        cout << i << ".";
        for(int j = 0; j < size_final; j++)
        {

            cout.width(3);
            cout << graph_final[i][j];
        }
        cout << endl;
    }
    cout << endl;
}


void print_dist()
{
    cout.width(3);
    cout << "x.";
    for(int i = 0; i < size_final; i++)
    {
        cout.width(3);
        cout << i;
    }
    cout << endl;

    for(int i = 0; i < size_final; i++)
    {
        cout.width(3);
        cout << i << ".";
        for(int j = 0; j < size_final; j++)
        {

            cout.width(3);
            cout << dist[i][j];
        }
        cout << endl;
    }
    cout << endl;
}

void print_dist_file(string fname)
{
    ofstream out("graphs/" + fname);

    for(int i = 0; i < size_final; i++)
    {
        for(int j = 0; j < size_final; j++)
            out << dist[i][j] << " ";
        out << endl;
    }
    out << endl;

    out.close();
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

template <typename T>
void print_vector_newline(vector<T> &a, string fname)
{
    //cout << "Size: " << a.size() << endl;

    ofstream out("entities/" + fname);
    for(int i = 0; i < a.size(); i++)
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
    for(int i = 0; i < s_final.size(); i++)
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

    in.open("data/POS.txt");
    while(getline(in, vlez))
    {
        POS.push_back(vlez);
        POS_aggr.push_back(vlez);
    }
    in.close();

    in.open("data/is_food.txt");
    while(in >> a)
    {
        is_food.push_back(a);
    }
    in.close();

    in.open("data/depidx.txt");
    while(in >> a)
    {
        dep.push_back(a);
    }
    in.close();

    in.open("data/govidx.txt");
    while(in >> a)
    {
        gov.push_back(a);
    }
    in.close();

    in.open("data/beginnings.txt");
    while(in >> a)
    {
        beginnings.push_back(a);
    }
    in.close();
}

void make_graph_v0()
{
    for(int i = 0; i < dep.size(); i++)
    {
        graph[dep[i]-1][gov[i]-1] = 1;
        graph[gov[i]-1][dep[i]-1] = 1;
    }


    bool curr_food = false, prev_food = false;

    //REWORK AS A DFA

    /*
        start from end, find food entity, then allow only N(but not O1), Z99 or JJ
        mark them to join, then join


    */
    for(int i = 0; i < s.size()-1; i++)
    {
        prev_food = (food_to_tag.count(s[i]) > 0);
        if(!present_row[i] || (POS[i].find("V") == 0) || (POS[i].find("CC") == 0) || (POS[i].find("MC") == 0) || (POS[i].find("MD") == 0) || (POS[i].find("MF") == 0) )
        {
            continue;
        }

        for(int j = i+1; j < s.size(); j++)
        {
            if(word_chunks[i] != word_chunks[j])
            {
                break;
            }

            curr_food = (food_to_tag.count(s[j]) > 0);

            if(!present_row[j] || (POS[j].find("V") == 0) || (POS[j].find("CC") == 0) || (POS[j].find("MC") == 0) || (POS[j].find("MD") == 0) || (POS[j].find("MF") == 0))
            {
                break;
                //continue;
            }

            if(prev_food && !curr_food)
                break;


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

            s_aggr[i] += " " + s_aggr[j];
            POS_aggr[i] += " " + POS_aggr[j];
            present_row[j] = false;
            present_col[j] = false;

        }
    }


    int koj = 0, koj2 = 0;
    for(int i = 0; i < s.size(); i++)
    {
        if(!present_row[i])
            continue;

        s_final.push_back(s_aggr[i]);
        POS_final.push_back(POS_aggr[i]);

        koj2 = 0;
        for(int j = 0; j < s.size(); j++)
        {
            if(!present_col[j])
                continue;

            graph_final[koj][koj2] = graph[i][j];
            koj2++;
        }
        koj++;
    }
    size_final = koj;


    for(int i = 0; i < size_final; i++)
    {

        graph_final[i][i] = 0;

    }
}

bool check(int i)
{
    if(!present_row[i])
        return false;

    // try J instead of JJ
    if(POS[i].find("JJ") == string::npos && POS[i].find("NP") == string::npos && POS[i].find("Z99") == string::npos && food_to_tag.count(s[i]) == 0)
        return(false);

    return(true);


}

void make_graph()
{
    for(int i = 0; i < dep.size(); i++)
    {
        graph[dep[i]-1][gov[i]-1] = 1;
        graph[gov[i]-1][dep[i]-1] = 1;
    }


    for(int i = 0; i < s.size()-1; i++)
    {
        if(!check(i))
        {
            continue;
        }

        for(int j = i+1; j < s.size(); j++)
        {
            if(word_chunks[i] != word_chunks[j])
            {
                break;
            }



            if(!check(j))
            {
                break;
                //continue;
            }


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

            s_aggr[i] += " " + s_aggr[j];
            POS_aggr[i] += " " + POS_aggr[j];
            present_row[j] = false;
            present_col[j] = false;

        }
    }


    int koj = 0, koj2 = 0;
    for(int i = 0; i < s.size(); i++)
    {
        if(!present_row[i])
            continue;

        s_final.push_back(s_aggr[i]);
        POS_final.push_back(POS_aggr[i]);

        koj2 = 0;
        for(int j = 0; j < s.size(); j++)
        {
            if(!present_col[j])
                continue;

            graph_final[koj][koj2] = graph[i][j];
            koj2++;
        }
        koj++;
    }
    size_final = koj;


    for(int i = 0; i < size_final; i++)
    {

        graph_final[i][i] = 0;

    }
}


void init()
{
    for(int i = 0;i < N; i++)
    {

        for(int j = 0; j < N; j++)
            graph[i][j] = 0;
    }

    // preprocessing - start

    for(int i = 0; i < beginnings.size()-1; i++)
    {

        for(int j = 0; j < beginnings[i+1] - beginnings[i]; j++)
        {
            word_chunks.push_back(beginnings[i]);
            //is_food_dupl.push_back(is_food[i]);
        }
    }


    for(int j = 0; j < s.size() - beginnings[beginnings.size()-1] + 1; j++)
    {
        word_chunks.push_back(beginnings[beginnings.size()-1]);
        //is_food_dupl.push_back(is_food[beginnings.size()-1]);
    }

    for(int i = 0; i < s.size(); i++)
    {
    	//TODO: FIX so it doesn't split compund nouns (NN + NN)
        if(is_food[i] && POS[i].find("N") == 0)
        {
            //cout << s[i] << endl;
            food_to_tag[s[i]] = true;
        }
    }
}

void change_sign()
{
    for(int i = 0; i < size_final; i++)
    {
        for(int j = 0; j < size_final; j++)
            graph_final[i][j] *= -1;
    }
}

void FW()
{
    for(int i = 0; i < size_final; i++)
    {
        for(int j = 0; j < size_final; j++)
        {
            if(graph_final[i][j] == 0)
                dist[i][j] = INT_MAX/2;
            else
                dist[i][j] = graph_final[i][j];
        }
        dist[i][i] = 0;
    }

    change_sign();
    int n = size_final, d;
    for(int k = 0; k < n; k++)
    {
        for(int i = 0; i < n; i++)
        {
            for(int j = 0; j < n; j++)
            {
                d = dist[i][k] + dist[k][j];
                if(d < dist[i][j])
                {
                    dist[i][j] = d;

                }
            }
        }
    }
    change_sign();
}


void make_pairs(int d_max)
{
    int n = size_final;


    for(int k = 1; k <= d_max; k++)
    {

        cout << "-------------Distance " << k << "-------------" << endl;
        for(int i = 0; i < n; i++)
        {
            if(!is_food_final[i])
                continue;

            cout << s_final[i] << endl;

            for(int j = 0; j < n; j++)
            {
                if(dist[i][j] == k)
                    cout << "    " << s_final[j] << endl;
            }
        }
        cout << endl;
    }
}

void make_pairs_file(int d_max, string fname)
{
    int n = size_final;

    ofstream out("outputs/" + fname);

    for(int k = 1; k <= d_max; k++)
    {

        out << "-------------Distance " << k << "-------------" << endl;
        for(int i = 0; i < n; i++)
        {
            if(!is_food_final[i])
                continue;

            out << s_final[i] << endl;

            for(int j = 0; j < n; j++)
            {
                if(dist[i][j] == k)
                    out << "    " << s_final[j] << endl;
            }
        }
        out << endl;
    }
    out.close();
}

void find_foods()
{
    bool found = false;
    string zbor;
    stringstream ss;
    for(int i = 0; i < size_final; i++)
    {
        found = false;
        ss.clear();
        ss << s_final[i];

        while(ss >> zbor)
        {
            if(food_to_tag.count(zbor) > 0)
            {
                found = true;
                break;
            }
        }

        if(found)
        {
            is_food_final.push_back(1);
        }
        else
            is_food_final.push_back(0);
    }

}


int main(int argc,char* argv[])
{
    int max_dist;
    if(argc < 2)
    {
        max_dist = 1;
    }
    else
    {
        max_dist = atoi(argv[1]);
    }

    get_input();
    init();
    make_graph();
    FW();
    find_foods();


    if(argc < 3)
    {
        //make_pairs(max_dist);
        print_vector_newline(s_final);
        print_vector_newline(POS_final);

    }
    else
    {
//        print_dist_file(argv[2]);
        print_vector_newline(s_final, argv[2]);
        print_graph_file(argv[2]);
        make_pairs_file(max_dist, argv[2]);
    }

    return(0);
}
