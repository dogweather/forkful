---
title:                "CSV এর সাথে কাজ করা"
date:                  2024-03-17T18:28:28.953855-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
CSV (কমা-সেপারেটেড ভ্যালুজ) নিয়ে কাজ করা মানে হল সিএসভি ফাইলে ডেটা পড়া এবং লিখা যা ট্যাবুলার ডেটা সংরক্ষণের জন্য একটি সাধারণ ফর্ম্যাট। প্রোগ্রামাররা এটি করে থাকেন সহজেই ডেটা আদান-প্রদান এবং সহজ, টেক্সট-ভিত্তিক ফর্ম্যাটে ডেটা সংরক্ষণের জন্য যা বিভিন্ন প্ল্যাটফর্ম ও ভাষায় ব্যাপকভাবে সমর্থিত।

## কিভাবে:
পাইথন সিএসভি ফাইলের জন্য বিল্ট-ইন `csv` মডিউল প্রদান করে থাকে, যা তাদের থেকে পড়া এবং তাদের মধ্যে লিখতে সহজ করে তোলে। আরও জটিল এবং দৃঢ় ডেটা ম্যানিপুলেশনের জন্য, তৃতীয়-পক্ষের লাইব্রেরি `pandas` অত্যন্ত জনপ্রিয়।

### `csv` মডিউল ব্যবহার করে

#### একটি CSV ফাইল পড়া
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*ধরুন `sample.csv` ফাইলে আছে:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*আউটপুট:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### একটি CSV ফাইলে লিখা
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*`output.csv` ফাইলটি তৈরি করে বা ওভাররাইট করে:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### CSV এর জন্য `pandas` ব্যবহার করা
`pandas` হলো একটি শক্তিশালী ডেটা ম্যানিপুলেশন লাইব্রেরি যা CSV ফাইলগুলির সাথে কাজ করা সহজ করে দেয় এবং অন্যান্য ডেটা ফর্ম্যাটের মধ্যে থাকে।

#### পান্ডাস ইনস্টল করা
```shell
pip install pandas
```

#### পান্ডাস দিয়ে একটি CSV ফাইল পড়া
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*আউটপুট:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### পান্ডাস দিয়ে একটি CSV ফাইলে লিখা
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*`output_pandas.csv` ফাইলটি তৈরি করে বা ওভাররাইট করে:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
