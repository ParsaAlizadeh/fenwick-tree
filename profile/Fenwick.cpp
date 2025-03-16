#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

// Reference: https://en.wikipedia.org/wiki/Fenwick_tree
template <class U> 
struct fenwick_tree {

  public:
    fenwick_tree() : _n(0) {}
    fenwick_tree(int n) : _n(n), data(n) {}

    void add(int p, U x) {
        // assert(0 <= p && p < _n);
        p++;
        while (p <= _n) {
            data[p - 1] += U(x);
            p += p & -p;
        }
    }

    U sum(int l, int r) {
        // assert(0 <= l && l <= r && r <= _n);
        return sum(r) - sum(l);
    }

    U sum(int r) {
        U s = 0;
        while (r > 0) {
            s += data[r - 1];
            r -= r & -r;
        }
        return s;
    }

  private:
    int _n;
    std::vector<U> data;
};

#define endl '\n'

int main() {
    cin.tie(nullptr)->sync_with_stdio(false);
    int n;
    cin >> n;
    // scanf(" %d", &n);
    fenwick_tree<ll> fen(n);
    while (true) {
        int typ;
        cin >> typ;
        // scanf(" %d", &typ);
        if (typ == -1)
            break;
        switch (typ) {
        case 0: {
            int p;
            ll x;
            cin >> p >> x;
            // scanf(" %d %lld", &p, &x);
            fen.add(p, x);
            break;
        }
        case 1: {
            int r;
            cin >> r;
            // scanf(" %d", &r);
            cout << fen.sum(r) << endl;
            // printf("%lld\n", fen.sum(r));
            break;
        }
        case 2: {
            int l, r;
            cin >> l >> r;
            // scanf(" %d %d", &l, &r);
            cout << fen.sum(l, r) << endl;
            // printf("%lld\n", fen.sum(l, r));
            break;
        }
        }
    }
    return 0;
}

