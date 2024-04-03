---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:20:13.093154-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 \u09A8\
  \u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B8\u09BF\u09AE\u09CD\u09AA\u09B2\
  \ \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BF\u0964."
lastmod: '2024-03-17T18:47:44.227438-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 \u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\
  \u09B2\u09CD\u09AA\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09BF\u09AE\u09CD\u09AA\u09B2 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\
  \u09CD\u099F \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BF\u0964."
title: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE"
weight: 1
---

## কিভাবে:
চলুন নতুন প্রকল্পের জন্য একটি সিম্পল স্ক্রিপ্ট তৈরি করি।

```Bash
#!/bin/bash

# প্রকল্প সেটআপ স্ক্রিপ্ট

PROJECT_NAME=$1
BASE_DIR=$(pwd)

# ডিরেক্টরিগুলি তৈরির ফাংশন
make_directories() {
    mkdir -p $PROJECT_NAME/{bin,src,doc,test}
    echo "ডিরেক্টরিগুলি তৈরি হয়েছে।"
}

# প্রাথমিক ফাইলগুলি তৈরির ফাংশন
make_files() {
    touch $PROJECT_NAME/README.md
    touch $PROJECT_NAME/src/main.sh
    echo "#!/bin/bash" > $PROJECT_NAME/src/main.sh
    chmod +x $PROJECT_NAME/src/main.sh
    echo "প্রাথমিক ফাইলগুলি তৈরি হয়েছে।"
}

# একটি গিট রিপোজিটরি ইনিশিয়ালাইজ করার ফাংশন
init_git() {
    cd $PROJECT_NAME
    git init
    cd $BASE_DIR
    echo "গিট রিপোজিটরি ইনিশিয়ালাইজ করা হয়েছে।"
}

# মূল নির্বাহণ
if [ -z "$PROJECT_NAME" ]; then
    echo "একটি প্রকল্পের নাম উল্লেখ করুন।"
else
    make_directories
    make_files
    init_git
    echo "'$PROJECT_NAME' প্রকল্পটি তৈরি করা হয়েছে।"
fi
```
`bash setup.sh myproject` চালানোর পরে নমুনা আউটপুট:

```Bash
ডিরেক্টরিগুলি তৈরি হয়েছে।
প্রাথমিক ফাইলগুলি তৈরি হয়েছে।
/path/to/myproject/.git/ এ খালি গিট রিপোজিটরি ইনিশিয়ালাইজ করা হয়েছে।
'myproject' প্রকল্পটি তৈরি করা হয়েছে।
```

## গভীর ডুব
স্ক্রিপ্ট থাকার আগে, আমরা প্রতি বারে ম্যানুয়ালি ডিরেক্টরি এবং ফাইলগুলি তৈরি করতাম— যা ক্লান্তিকর এবং ভুলের সম্ভাবনা পূর্ণ। স্ক্রিপ্টের সাহায্যে অটোমেশন ভুল কমাতে এবং গতি বাড়াতে সাহায্য করে।

বিকল্পগুলি হল Yeoman এর মতো টুলস, যা বিভিন্ন ভাষায় প্রকল্প তৈরি করে, কিন্তু তা হল যেন একটা পাওয়ার ড্রিল ব্যবহার করা যখন আপনার একটি থাম্বট্যাক দরকার।

উপরের স্ক্রিপ্টটি ইচ্ছাকৃতভাবে সাধারণ রাখা হয়েছে। এটি একটি প্রকল্প ডিরেক্টরি, সংগঠনের জন্য উপ-ডিরেক্টরি (যেমন 'src' সোর্স কোডের জন্য), এবং প্রয়োজনীয় ফাইলগুলি (যেমন 'README.md') তৈরি করে। তাছাড়া, এটি একটি গিট রেপো সেটাপ করে, যাতে আপনি আপনার কাজের ভার্সনগুলি সংরক্ষণ করতে পারেন। আপনি প্রতিটি প্রকল্পের প্রয়োজনে এটিকে টুইক এবং যোগ করতে পারেন।

## আরও দেখুন
- গিট ডকুমেন্টেশন: https://git-scm.com/doc
- Yeoman: http://yeoman.io/
- ব্যাশ স্ক্রিপ্টিং টিউটোরিয়াল: https://www.shellscript.sh/
