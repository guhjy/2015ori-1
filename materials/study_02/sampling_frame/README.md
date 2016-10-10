In order to go from `savedrecs.txt` to `full_frame.csv` the following terminal script was used

```bash
echo emails > full_frame.csv
cat savedrecs.txt | grep -P 'EM\s' | sed 's/EM\s//g' | sed 's/;//g' | tr '[:upper:]' '[:lower:]' | sed 's/\s/\n/g' | sort | uniq >> full_frame.csv
```
