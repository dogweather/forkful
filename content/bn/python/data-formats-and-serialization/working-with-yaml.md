---
title:                "ইয়ামেল নিয়ে কাজ করা"
date:                  2024-03-17T18:37:51.218852-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
