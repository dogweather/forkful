---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:19.880122-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: **\u09B2\u09BE\u0987\u09A8 \u09A6\
  \u09CD\u09AC\u09BE\u09B0\u09BE \u09B2\u09BE\u0987\u09A8 CSV \u09AB\u09BE\u0987\u09B2\
  \ \u09AA\u09A1\u09BC\u09BE**."
lastmod: '2024-03-17T18:47:44.250521-06:00'
model: gpt-4-0125-preview
summary: "**\u09B2\u09BE\u0987\u09A8 \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u09B2\u09BE\
  \u0987\u09A8 CSV \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE**."
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:
**লাইন দ্বারা লাইন CSV ফাইল পড়া**

```bash
while IFS=, read -r column1 column2 column3
do
  echo "কলাম ১: $column1, কলাম ২: $column2, কলাম ৩: $column3"
done < sample.csv
```

*নমুনা আউটপুট:*

```
কলাম ১: id, কলাম ২: name, কলাম ৩: email
...
```

**একটি শর্তের ভিত্তিতে CSV সারিগুলি ফিল্টার করা**

`awk` ব্যবহার করে সহজেই সারি ফিল্টার করা যায়। উদাহরণস্বরূপ, দ্বিতীয় কলাম যেখানে "Alice" সমান:

```bash
awk -F, '$2 == "Alice" { print $0 }' sample.csv
```

**একটি কলাম মান পরিবর্তন করা**

দ্বিতীয় কলামকে আপারকেসে পরিবর্তন করতে:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' sample.csv
```

**একটি কলামের ভিত্তিতে CSV ফাইল সাজানো**

ধরুন, তৃতীয় কলামের ভিত্তিতে (সংখ্যাগতভাবে) একটি CSV ফাইল সাজাতে পারেন:

```bash
sort -t, -k3,3n sample.csv
```

**আরও জটিল কাজের জন্য `csvkit` ব্যবহার করা**

`csvkit` একটি কমান্ড-লাইন টুলসের সুইট যা CSV নিয়ে কাজ করতে এবং CSV-তে রূপান্তর করতে সহায়ক। এটি pip এর মাধ্যমে ইন্সটল করা যেতে পারে।

JSON ফাইলকে CSV-তে রূপান্তর করতে:

```bash
in2csv data.json > data.csv
```

SQL ব্যবহার করে CSV ফাইলে কোয়েরি করা:

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" sample.csv
```

*নোট: `csvkit` ইন্সটল করা পাইথনের প্রয়োজন এবং এটি করা যেতে পারে `pip install csvkit` ব্যবহার করে।*
