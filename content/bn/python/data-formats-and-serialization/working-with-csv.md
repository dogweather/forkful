---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:28.953855-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AA\u09BE\u0987\u09A5\u09A8\
  \ \u09B8\u09BF\u098F\u09B8\u09AD\u09BF \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 `csv` \u09AE\u09A1\
  \u09BF\u0989\u09B2 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7 \u09A5\
  \u09BE\u0995\u09C7, \u09AF\u09BE \u09A4\u09BE\u09A6\u09C7\u09B0 \u09A5\u09C7\u0995\
  \u09C7 \u09AA\u09A1\u09BC\u09BE \u098F\u09AC\u0982 \u09A4\u09BE\u09A6\u09C7\u09B0\
  \ \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B2\u09BF\u0996\u09A4\u09C7 \u09B8\u09B9\u099C\
  \ \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964 \u0986\u09B0\u0993 \u099C\u099F\
  \u09BF\u09B2 \u098F\u09AC\u0982 \u09A6\u09C3\u09A2\u09BC \u09A1\u09C7\u099F\u09BE\
  \u2026"
lastmod: '2024-03-17T18:47:43.598825-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0987\u09A5\u09A8 \u09B8\u09BF\u098F\u09B8\u09AD\u09BF \u09AB\
  \u09BE\u0987\u09B2\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\u09B2\u09CD\
  \u099F-\u0987\u09A8 `csv` \u09AE\u09A1\u09BF\u0989\u09B2 \u09AA\u09CD\u09B0\u09A6\
  \u09BE\u09A8 \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7, \u09AF\u09BE \u09A4\u09BE\
  \u09A6\u09C7\u09B0 \u09A5\u09C7\u0995\u09C7 \u09AA\u09A1\u09BC\u09BE \u098F\u09AC\
  \u0982 \u09A4\u09BE\u09A6\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B2\u09BF\
  \u0996\u09A4\u09C7 \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\
  \u0964 \u0986\u09B0\u0993 \u099C\u099F\u09BF\u09B2 \u098F\u09AC\u0982 \u09A6\u09C3\
  \u09A2\u09BC \u09A1\u09C7\u099F\u09BE \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\
  \u09C1\u09B2\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF, \u09A4\u09C3\
  \u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF `pandas` \u0985\u09A4\u09CD\u09AF\u09A8\u09CD\
  \u09A4 \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC\u0964\n\n\n"
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

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
