# betterDays
Takes in a .csv or .tsv table and transforms all columns that contains dates in a desordered format to :sparkles: yyyy-mm-dd :sparkles:

### Packages
```ggplot2``` must be installed before execution

### Usage
#### Linux
```
Rscript betterDates.R <input file> <output file>
```
#### Windows
If Rscript is not in path
```
"C:\Program Files\R\R-4.3.2\bin\Rscript.exe" betterDates.R <input file> <output file>
```

### Example
Input:
| Sample | id  | collection_date | sequencing_date | Passage  |
|--------|-----|-----------------|-----------------|----------|
| id1    | 445 | 2/12/2024       | 2024            | original |
| id2    | 447 | 2023            | 2023            | original |
| id2    | 449 | 2023-5-30       | 25/5/23         | original |

Output:
| Sample | id  | collection_date | sequencing_date | Passage  |
|--------|-----|-----------------|-----------------|----------|
| id1    | 445 | 2024-12-02       | 2024            | original |
| id2    | 447 | 2023            | 2023            | original |
| id2    | 449 | 2023-05-30       | 2023-05-23     | original |

