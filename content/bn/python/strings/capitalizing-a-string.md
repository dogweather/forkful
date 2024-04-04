---
changelog:
- 2024-04-04 - dogweather - edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: #."
lastmod: '2024-04-04T00:27:29.201185-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0995\u09C7 \u09AC\u09A1\u09BC\
  \ \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09B0\u09C2\
  \u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 2
---

## কিভাবে:

### পাইথনের অন্তর্নির্মিত পদ্ধতি ব্যবহার করে:
পাইথনে স্ট্রিংগুলি সহজে এই কাজটি সম্পন্ন করার জন্য একটি অন্তর্নির্মিত পদ্ধতি `.capitalize()` রয়েছে।

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**আউটপুট:**
```
Hello world
```

এখানে আমার নিজের কাস্টমাইজড `capitalize()` এটি এই সাইট নির্মাণের জন্য আমি ব্যবহার করেছি। আমাকে নিশ্চিত করতে হয়েছিল যে **HTML** এর মত বিশেষ শব্দগুলো সবসময়ে সকল অক্ষর বড় হাতের হয়ে থাকে। এটি [doctests](https://docs.python.org/3/library/doctest.html) দেখানোর একটি প্রমাণ ও হোল:

```python
def capitalize(string: str) -> str:
    """
    একটি স্ট্রিংকে বড় হাতের অক্ষরে লিখুন, অর্থাৎ প্রথম অক্ষরটিকে উপরের কেসে করুন।
    "HTML" এর মত বিশেষ ক্ষেত্রসমূহ আলাদা করে চিনুন।

    >>> capitalize("this is html, csv, xml, and http (no REPL).")
    'This is HTML, CSV, XML, and HTTP (no REPL).'

    >>> capitalize("this is json, VBA, an IDE, and yaml in the CLI.")
    'This is JSON, VBA, an IDE, and YAML in the CLI.'
    """
    return (
        string
            .capitalize()
            .replace('cli',  'CLI')
            .replace('csv',  'CSV')
            .replace('html', 'HTML')
            .replace('http', 'HTTP')
            .replace('ide',  'IDE')
            .replace('json', 'JSON')
            .replace('repl', 'REPL')
            .replace('vba',  'VBA')
            .replace('xml',  'XML')
            .replace('yaml', 'YAML')
    )

```

### একাধিক শব্দ সামলানো:
যে সব স্থিতিতে আপনি চান একটি স্ট্রিংয়ের প্রতিটি শব্দ বড় হাতের অক্ষরে শুরু হোক (যেমন টাইটেলগুলি), এর জন্য `.title()` পদ্ধতিটি প্রযোজ্য হতে পারে।

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**আউটপুট:**
```
Python Programming Essentials
```

### থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করে:
যদিও পাইথনের স্ট্যান্ডার্ড লাইব্রেরি বেসিক স্ট্রিং ক্যাপিটালাইজেশনের জন্য সজ্জিত, `textblob` এর মত লাইব্রেরিগুলি প্রাকৃতিক ভাষা প্রক্রিয়াকরণের জন্য আরও নির্দৃষ্ট নিয়ন্ত্রণ প্রদান করতে পারে।

প্রথমে, নিশ্চিত করুন আপনি `textblob` ইনস্টল করেছেন:
```bash
pip install textblob
```

তারপর, কনটেক্সট অনুযায়ী `textblob`'এর ক্যাপিটালাইজ কিভাবে কাজ করে তা মাথায় রেখে স্ট্রিংগুলি ক্যাপিটালাইজ করতে এটি ব্যবহার করুন:

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**আউটপুট:**
```
This is a test sentence
```

মনে রাখবেন, `capitalize()` এবং `title()` পদ্ধতিগুলি সর্বজনীনভাবে উপকারী হলেও, `textblob` এর মত লাইব্রেরিগুলি বিশেষ অ্যাপ্লিকেশনগুলির জন্য অতিরিক্ত নমনীয়তা প্রদান করতে পারে।
