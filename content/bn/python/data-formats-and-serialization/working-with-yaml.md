---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:51.218852-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Python \u098F YAML \u09AA\u09A1\
  \u09BC\u09BE \u098F\u09AC\u0982 \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u09A4\u09C3\u09A4\u09C0\u09AF\
  \u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u09AA\u09CD\
  \u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09B9\u09AF\u09BC, `PyYAML` \u09B8\u09AC\u099A\
  \u09C7\u09AF\u09BC\u09C7 \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC\u0964\
  \ \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u0986\
  \u09AA\u09A8\u09BE\u0995\u09C7 `pip\u2026"
lastmod: '2024-03-17T18:47:43.596694-06:00'
model: gpt-4-0125-preview
summary: "Python \u098F YAML \u09AA\u09A1\u09BC\u09BE \u098F\u09AC\u0982 \u09B2\u09C7\
  \u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\
  \u09A4 \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09B9\
  \u09AF\u09BC, `PyYAML` \u09B8\u09AC\u099A\u09C7\u09AF\u09BC\u09C7 \u099C\u09A8\u09AA\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u0964 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF, \u0986\u09AA\u09A8\u09BE\u0995\u09C7 `pip install\
  \ PyYAML` \u099A\u09BE\u09B2\u09BE\u09A8\u09CB\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 PyYAML \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09BE \u09AA\
  \u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u0964\n\n**\u0989\u09A6\u09BE\u09B9\u09B0\
  \u09A3."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

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
