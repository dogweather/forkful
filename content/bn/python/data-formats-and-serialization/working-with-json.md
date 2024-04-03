---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:52.602669-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AA\u09BE\u0987\u09A5\u09A8\
  \u09C7\u09B0 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 `json` \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AA\u09BE\u0987\u09A5\u09A8 \u0985\u09AC\
  \u099C\u09C7\u0995\u09CD\u099F\u0997\u09C1\u09B2\u09BF\u0995\u09C7 JSON \u098F \u098F\
  \u09A8\u0995\u09CB\u09A1\u09BF\u0982 (\u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\
  \u09A8) \u098F\u09AC\u0982 JSON \u09A5\u09C7\u0995\u09C7 \u09AA\u09BE\u0987\u09A5\
  \u09A8 \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u09C7 \u09A1\u09BF\u0995\u09CB\
  \u09A1\u09BF\u0982 (\u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8) \u0995\u09B0\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09C7\
  \u2026"
lastmod: '2024-03-17T18:47:43.597733-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7\u09B0 \u09AC\u09BF\u09B2\u09CD\u099F\
  -\u0987\u09A8 `json` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AA\
  \u09BE\u0987\u09A5\u09A8 \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u0997\u09C1\u09B2\
  \u09BF\u0995\u09C7 JSON \u098F \u098F\u09A8\u0995\u09CB\u09A1\u09BF\u0982 (\u09AA\
  \u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8) \u098F\u09AC\u0982 JSON \u09A5\u09C7\
  \u0995\u09C7 \u09AA\u09BE\u0987\u09A5\u09A8 \u0985\u09AC\u099C\u09C7\u0995\u09CD\
  \u099F\u09C7 \u09A1\u09BF\u0995\u09CB\u09A1\u09BF\u0982 (\u09AA\u09B0\u09BF\u09AC\
  \u09B0\u09CD\u09A4\u09A8) \u0995\u09B0\u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\
  \u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09C7 \u09B8\u09B9\u099C \u0995\u09B0\u09C7\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\
  \u09B2 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF \u098F\u099F\
  \u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u09A8."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কিভাবে:
পাইথনের বিল্ট-ইন `json` লাইব্রেরি পাইথন অবজেক্টগুলিকে JSON এ এনকোডিং (পরিবর্তন) এবং JSON থেকে পাইথন অবজেক্টে ডিকোডিং (পরিবর্তন) করার প্রক্রিয়াকে সহজ করে। এখানে দেখানো হল কিভাবে আপনি এটি ব্যবহার করতে পারেন:

### পাইথন অবজেক্টগুলিকে JSON এ এনকোডিং:
```python
import json

data = {
    "name": "John Doe",
    "age": 30,
    "isEmployee": True,
    "addresses": [
        {"city": "New York", "zipCode": "10001"},
        {"city": "San Francisco", "zipCode": "94016"}
    ]
}

json_string = json.dumps(data, indent=4)
print(json_string)
```

**আউটপুট:**

```json
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
```

### JSON কে পাইথন অবজেক্টে ডিকোডিং:
```python
json_string = '''
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
'''

data = json.loads(json_string)
print(data)
```

**আউটপুট:**

```python
{
    'name': 'John Doe', 
    'age': 30, 
    'isEmployee': True, 
    'addresses': [
        {'city': 'New York', 'zipCode': '10001'}, 
        {'city': 'San Francisco', 'zipCode': '94016'}
    ]
}
```

### তৃতীয় পার্টি লাইব্রেরিগুলির সাথে কাজ করা:
জটিল JSON হ্যান্ডেলিং জন্য, যেমন স্কিমা ভ্যালিডেশন বা URL-গুলি থেকে সরাসরি JSON ফাইল পার্সিং, `requests` লাইব্রেরি HTTP অনুরোধের জন্য এবং `jsonschema` ভ্যালিডেশনের জন্য উপকারী হতে পারে।

#### URL থেকে JSON পার্স করার জন্য `requests` এর সাথে উদাহরণ:
```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

এই স্নিপেটটি একটি নির্দিষ্ট URL থেকে JSON ডাটা আনে এবং সরাসরি এটি একটি পাইথন অবজেক্টে পরিবর্তন করে।

#### JSON ভ্যালিডেট করার জন্য `jsonschema` ব্যবহার করা:
প্রথমে, পিপের মাধ্যমে লাইব্রেরিটি ইনস্টল করুন:

```bash
pip install jsonschema
```

তারপর, এটি নিম্নলিখিতভাবে ব্যবহার করুন:

```python
from jsonschema import validate
import jsonschema

schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "number"},
        "isEmployee": {"type": "boolean"},
    },
    "required": ["name", "age", "isEmployee"]
}

# ধরে নেওয়া হচ্ছে `data` হল একটি JSON ডিকোড করা ডিকশনারি
try:
    validate(instance=data, schema=schema)
    print("ভ্যালিড JSON ডাটা।")
except jsonschema.exceptions.ValidationError as err:
    print("ভ্যালিডেশন ত্রুটি:", err)
```

এই উদাহরণটি আপনার পাইথন ডিকশনারিকে (যা ডিকোডেড JSON ডাটা থেকে প্রাপ্ত) একটি পূর্বনির্ধারিত স্কিমার বিরুদ্ধে ভ্যালিডেট করে, নিশ্চিত করে যে ডাটা প্রত্যাশিত ফর্ম্যাট এবং টাইপের সাথে মিলে যায়।
