---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:08.394426-06:00
description: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 HTML \u0995\u09A8\u099F\u09C7\u09A8\u09CD\u099F \u09A5\u09C7\u0995\u09C7\
  \ \u09A1\u09C7\u099F\u09BE \u09AC\u09BE \u09A4\u09A5\u09CD\u09AF \u098F\u0995\u09CD\
  \u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE, \u09AF\u09BE\
  \ \u0993\u09AF\u09BC\u09C7\u09AC \u09A1\u09C7\u099F\u09BE \u09A8\u09BF\u09AF\u09BC\
  \u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u09B8\u09AE\u09AF\u09BC \u098F\
  \u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u0995\u09BE\u099C\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BE \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.495393-06:00'
model: gpt-4-0125-preview
summary: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 HTML \u0995\u09A8\u099F\u09C7\u09A8\u09CD\u099F \u09A5\u09C7\u0995\u09C7\
  \ \u09A1\u09C7\u099F\u09BE \u09AC\u09BE \u09A4\u09A5\u09CD\u09AF \u098F\u0995\u09CD\
  \u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE, \u09AF\u09BE\
  \ \u0993\u09AF\u09BC\u09C7\u09AC \u09A1\u09C7\u099F\u09BE \u09A8\u09BF\u09AF\u09BC\
  \u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u09B8\u09AE\u09AF\u09BC \u098F\
  \u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u0995\u09BE\u099C\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BE \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7 \u0993\u09AF\u09BC\
  \u09C7\u09AC\u09B8\u09BE\u0987\u099F \u09A5\u09C7\u0995\u09C7 \u09A4\u09A5\u09CD\
  \u09AF \u0985\u099F\u09CB\u09AE\u09C7\u099F\u09C7\u09A1\u09AD\u09BE\u09AC\u09C7\
  \ \u098F\u0995\u09CD\u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u09AF\u09C7\u09AE\u09A8 \u0993\u09AF\u09BC\
  \u09C7\u09AC \u09B8\u09CD\u0995\u09CD\u09B0\u09CD\u09AF\u09BE\u09AA\u09BF\u0982\
  , \u09A1\u09C7\u099F\u09BE \u09AE\u09BE\u0987\u09A8\u09BF\u0982, \u09AC\u09BE \u0985\
  \u099F\u09CB\u09AE\u09C7\u099F\u09C7\u09A1 \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982\
  \ \u098F\u09B0 \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\u0964."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কিভাবে:
Fish shell, মূলত, সরাসরি HTML পার্স করার জন্য ডিজাইন করা হয়নি। তবে, এটি `curl`, `grep`, `sed`, `awk`, বা `pup` বা Python স্ক্রিপ্টে `beautifulsoup` এর মতো বিশেষায়িত টুলগুলির সঙ্গে Unix টুলগুলি একত্রিত করে দুর্দান্ত কাজ করে। নিচে উদাহরণ দেওয়া হল যেগুলি দেখায় যে Fish shell এর মধ্যে থেকে এই টুলগুলি কিভাবে HTML পার্স করার জন্য ব্যবহার করা যায়।

### `curl` এবং `grep` ব্যবহার করে:
HTML কনটেন্ট ফেচ করে এবং লিংকসমূহ থাকা লাইনগুলি এক্সট্রাক্ট করা:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

আউটপুট:
```
/page1.html
/page2.html
...
```

### `pup` ব্যবহার করে (HTML পার্স করার জন্য একটি কমান্ড-লাইন টুল):
প্রথমে, নিশ্চিত করুন যে `pup` ইন্সটল করা আছে। তারপর আপনি তার ট্যাগ, আইডি, ক্লাস, ইত্যাদি দ্বারা এলিমেন্টগুলি এক্সট্রাক্ট করতে এটি ব্যবহার করতে পারবেন।

```fish
curl -s https://example.com | pup 'a attr{href}'
```

`grep` উদাহরণের মতো, আউটপুটে `<a>` ট্যাগের href অ্যাট্রিবিউটগুলি তালিকাভুক্ত হবে।

### একটি Python স্ক্রিপ্ট এবং `beautifulsoup` ব্যবহার করে:
যদিও Fish নিজে নেটিভভাবে HTML পার্স করতে পারে না, এটি Python স্ক্রিপ্টের সাথে নির্বিঘ্নে ইন্টিগ্রেট করতে পারে। নিচে একটি সংক্ষিপ্ত উদাহরণ দেওয়া হল যা Python এবং `BeautifulSoup` ব্যবহার করে HTML থেকে টাইটেল এক্সট্রাক্ট করার জন্য। নিশ্চিত করুন যে আপনার Python পরিবেশে `beautifulsoup4` এবং `requests` ইন্সটল করা আছে।

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

ব্যবহার:

```fish
parse_html 'https://example.com'
```

আউটপুট:
```
Example Domain
```

এই পদ্ধতিগুলি বিভিন্ন ব্যবহারের ক্ষেত্র এবং জটিলতার মাত্রা সার্ভ করে, সাধারণ কমান্ড-লাইন টেক্সট ম্যানিপুলেশন থেকে শুরু করে Python স্ক্রিপ্টের মধ্যে `beautifulsoup` এর পূর্ণ পার্সিং ক্ষমতা পর্যন্ত। আপনার প্রয়োজন এবং HTML স্ট্রাকচারের জটিলতা অনুসারে, আপনি একটি সোজাসাপ্টা Unix পাইপলাইন বা আরও শক্তিশালী স্ক্রিপ্টিং পদ্ধতি বেছে নিতে পারেন।
