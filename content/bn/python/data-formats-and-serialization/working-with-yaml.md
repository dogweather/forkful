---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:51.218852-06:00
description: "YAML, \u09AF\u09BE\u09B0 \u09AA\u09C2\u09B0\u09CD\u09A3 \u09B0\u09C2\
  \u09AA \u09B9\u09B2 YAML Ain't Markup Language, \u098F\u0995\u099F\u09BF \u09AE\u09BE\
  \u09A8\u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF \u09A4\u09A5\u09CD\u09AF \u09B8\u09BF\
  \u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AB\u09B0\
  \u09AE\u09CD\u09AF\u09BE\u099F\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE XML \u0985\u09A5\u09AC\u09BE JSON \u098F\u09B0 \u09AE\
  \u09A4 \u0985\u09A8\u09CD\u09AF\u09BE\u09A8\u09CD\u09AF \u09AB\u09B0\u09CD\u09AE\
  \u09CD\u09AF\u09BE\u099F\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.596694-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u09AF\u09BE\u09B0 \u09AA\u09C2\u09B0\u09CD\u09A3 \u09B0\u09C2\u09AA\
  \ \u09B9\u09B2 YAML Ain't Markup Language, \u098F\u0995\u099F\u09BF \u09AE\u09BE\
  \u09A8\u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF \u09A4\u09A5\u09CD\u09AF \u09B8\u09BF\
  \u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AB\u09B0\
  \u09AE\u09CD\u09AF\u09BE\u099F\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE XML \u0985\u09A5\u09AC\u09BE JSON \u098F\u09B0 \u09AE\
  \u09A4 \u0985\u09A8\u09CD\u09AF\u09BE\u09A8\u09CD\u09AF \u09AB\u09B0\u09CD\u09AE\
  \u09CD\u09AF\u09BE\u099F\u09C7\u09B0\u2026"
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

## কি এবং কেন?
YAML, যার পূর্ণ রূপ হল YAML Ain't Markup Language, একটি মানব-পাঠ্য তথ্য সিরিয়ালাইজেশন ফরম্যাট। প্রোগ্রামাররা XML অথবা JSON এর মত অন্যান্য ফর্ম্যাটের তুলনায় এর সহজ সিনট্যাক্স এবং সহজে পাঠ্যতা এর কারণে YAML ব্যবহার করে কনফিগারেশন ফাইল, ইন্টার-প্রসেস মেসেজিং, এবং ডাটা স্টোরেজের জন্য।

## কিভাবে:
Python এ YAML পড়া এবং লেখার জন্য সাধারণত তৃতীয়-পক্ষের লাইব্রেরির ব্যবহার প্রয়োজন হয়, `PyYAML` সবচেয়ে জনপ্রিয়। শুরু করার জন্য, আপনাকে `pip install PyYAML` চালানোর মাধ্যমে PyYAML ইনস্টল করা প্রয়োজন।

**উদাহরণ: YAML ফাইলে লেখা**

```python
import yaml

data = {'a list': [1, 42, 3.141, 1337, 'help', u'€'],
        'a string': 'boo!',
        'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}

with open('example.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# এটি `example.yaml` ফাইলটি YAML ফরম্যাটে তথ্য স্ট্রাকচার সহ তৈরি করে।
```

**উদাহরণ: YAML ফাইল থেকে পড়া**

```python
import yaml

with open('example.yaml', 'r') as f:
    data_loaded = yaml.safe_load(f)

print(data_loaded)

# আউটপুট: 
# {'a list': [1, 42, 3.141, 1337, 'help', '€'],
#  'a string': 'boo!',
#  'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}
```

**কনফিগারেশনে YAML ব্যবহার**

অনেক প্রোগ্রামার YAML ব্যবহার করে অ্যাপ্লিকেশন কনফিগারেশন পরিচালনা করে। এখানে একটি কনফিগ ফাইল কিভাবে স্ট্রাকচার করা যায় এবং পড়া যায় তার একটি উদাহরণ দেওয়া হল:

config.yaml:
```yaml
database:
  host: localhost
  port: 5432
  username: admin
  password: secret
```

Python এ কনফিগ ফাইল পড়া:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['database']['host'])  # আউটপুট: localhost
```

**জটিল স্ট্রাকচার হ্যান্ডেলিং**

জটিল স্ট্রাকচারের জন্য, PyYAML আপনাকে কাস্টম Python অবজেক্ট ডিফাইন করার অনুমতি দেয়। তবে, অপ্রত্যাশিত ফাংশন বা অবজেক্ট চালানো থেকে বাঁচতে `safe_load` ব্যবহার করা নিরাপদ।

```python
import yaml

# একটি Python অবজেক্ট ডিফাইন করুন
class Example:
    def __init__(self, value):
        self.value = value

# কাস্টম কনস্ট্রাক্টর
def constructor_example(loader, node):
    value = loader.construct_scalar(node)
    return Example(value)

# ট্যাগ "!example" এর জন্য কনস্ট্রাক্টর যোগ করুন
yaml.add_constructor('!example', constructor_example)

yaml_str = "!example 'data'"
loaded = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(loaded.value)  # আউটপুট: data
```

এই স্নিপেটে, `!example` হল একটি কাস্টম ট্যাগ যেটি একটি YAML স্ট্রিং থেকে মান 'data' সহ একটি `Example` অবজেক্ট ইন্সট্যানট করার জন্য ব্যবহৃত। এইরকম কাস্টম লোডারস PyYAML এর নমনীয়তা বাড়ায়, আরো জটিল ডাটা স্ট্রাকচার এবং টাইপগুলি প্রসেসিং এর অনুমতি দিয়ে।
