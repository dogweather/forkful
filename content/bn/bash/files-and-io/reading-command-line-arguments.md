---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:40.128071-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09A7\u09B0\u09C1\u09A8 \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7\
  \u09B0 \u09A8\u09BE\u09AE 'example.sh' \u098F\u09AC\u0982 \u0986\u09AA\u09A8\u09BF\
  \ `./example.sh arg1 arg2` \u0995\u09B2 \u0995\u09B0\u09C7\u099B\u09C7\u09A8."
lastmod: '2024-04-05T21:53:52.730457-06:00'
model: gpt-4-0125-preview
summary: "\u09A7\u09B0\u09C1\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u0995\
  \u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7\u09B0 \u09A8\u09BE\u09AE 'example.sh'\
  \ \u098F\u09AC\u0982 \u0986\u09AA\u09A8\u09BF `./example.sh arg1 arg2` \u0995\u09B2\
  \ \u0995\u09B0\u09C7\u099B\u09C7\u09A8."
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কিভাবে:
```Bash
#!/bin/bash

# স্ক্রিপ্টের নাম প্রিন্ট করুন।
echo "স্ক্রিপ্টের নাম: $0"

# প্রথম আর্গুমেন্ট প্রিন্ট করুন।
echo "প্রথম আর্গুমেন্ট: $1"

# সমস্ত আর্গুমেন্ট প্রিন্ট করুন।
echo "সমস্ত আর্গুমেন্ট: $@"
```

ধরুন আপনার স্ক্রিপ্টের নাম 'example.sh' এবং আপনি `./example.sh arg1 arg2` কল করেছেন:

```
স্ক্রিপ্টের নাম: ./example.sh
প্রথম আর্গুমেন্ট: arg1
সমস্ত আর্গুমেন্ট: arg1 arg2
```

আর্গুমেন্টগুলির মধ্যে লুপ করুন:

```Bash
#!/bin/bash

# প্রত্যেক আর্গুমেন্টের মধ্যে লুপ করুন।
for arg in "$@"; do
  echo "আর্গুমেন্ট: $arg"
done
```

## গভীর ডুব
Bash বহু বছর ধরে কমান্ড লাইন আর্গুমেন্টের সাপোর্ট করে; এগুলি হল পজিশনাল প্যারামিটার, `$0` থেকে `$9`, `$@` এবং `$*` সবকিছু দেখায়। `$0` হল খোদ স্ক্রিপ্টটি, `$1` থেকে `$9` হল প্রথম থেকে নবম আর্গুমেণ্ট; দশম এবং তারপর থেকে `${10}` এর মতো ব্রেসগুলির প্রয়োজন।

স্পেস সহ আর্গুমেন্টগুলি সঠিকভাবে হ্যান্ডল করার জন্য `$@` ব্যবহার করা সাধারণত `$*` এর চেয়ে ভাল। `$@` প্রতিটি আর্গুমেন্টকে পৃথক "শব্দ" হিসেবে দেয়; `$*` তাদের সবকিছুকে একটি একক "শব্দ" হিসেবে একত্রিত করে।

`shift` কমান্ড ব্যবহার করে আর্গুমেন্টের মধ্য দিয়ে সরানো যায়, যা `$2` কে `$1` এ পরিণত করে, এবং এভাবে আগের `$1` কে বাদ দেয়।

বিকল্পগুলি? নিশ্চিত। `getopts` এবং `getopt` অপশনগুলির জন্য (যেমন -h সাহায্যের জন্য) এবং পতাকা পারসিংয়ের জন্য আরও নিয়ন্ত্রণ প্রদান করে; `$1`, `$2`,... যদি যথেষ্ট না হয় তবে তাদের চেক করুন।

## আরও দেখুন
- বিশেষ প্যারামিটারগুলি প্রসঙ্গে বাশ ম্যানুয়াল: https://www.gnu.org/software/bash/manual/html_node/Special-Parameters.html
- উন্নত বাশ-স্ক্রিপ্টিং গাইড: https://www.tldp.org/LDP/abs/html/
- `getopts` টিউটোরিয়াল: https://wiki.bash-hackers.org/howto/getopts_tutorial
