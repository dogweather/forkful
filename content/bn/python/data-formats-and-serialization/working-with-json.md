---
title:                "JSON এর সাথে কাজ করা"
date:                  2024-03-17T18:29:52.602669-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

JSON (JavaScript Object Notation) এর সাথে কাজ করা মানে হল JSON বিন্যাসে লেখা স্ট্রিংকে পাইথন অবজেক্টে পরিবর্তন করা এবং উল্টো প্রক্রিয়াটিও সম্পাদন করা। ওয়েব এবং API ডেভেলপমেন্টে এটি অত্যন্ত জরুরি, কারণ JSON হল সার্ভার এবং ক্লায়েন্টের মধ্যে ডাটা বিনিময়ের জন্য মূল ভাষা।

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
