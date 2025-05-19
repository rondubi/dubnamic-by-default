
int main()
{
        int x = 0;
        int y = 2;
        for (int q = 0; q < 5; q++)
        {
                for (int j = q; j < 5; j++)
                {
                        x += j;
                        y += q;
                }
        }

        return 0;
}

