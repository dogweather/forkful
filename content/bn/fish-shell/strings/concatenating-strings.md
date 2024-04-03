---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:20.762086-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AB\u09BF\u09B6\u09C7, \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\u09CD\
  \u09AA\u09C7\u09B8 \u09A6\u09BF\u09AF\u09BC\u09C7 \u098F\u0995 \u09B8\u09BE\u09A5\
  \u09C7 \u099C\u09CB\u09A1\u09BC\u09BE \u09A6\u09BE\u0993 \u0985\u09A5\u09AC\u09BE\
  \ `string join` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09CB\u0964\
  ."
lastmod: '2024-03-17T18:47:44.487385-06:00'
model: gpt-4-0125-preview
summary: "\u09AB\u09BF\u09B6\u09C7, \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\
  \u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\u09CD\u09AA\u09C7\u09B8 \u09A6\u09BF\u09AF\
  \u09BC\u09C7 \u098F\u0995 \u09B8\u09BE\u09A5\u09C7 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09BE\u0993 \u0985\u09A5\u09AC\u09BE `string join` \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09CB\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কিভাবে:
ফিশে, স্ট্রিংগুলিকে স্পেস দিয়ে এক সাথে জোড়া দাও অথবা `string join` ব্যবহার করো।

```fish
# 'Hello' এবং 'World!' কে একটি স্পেস দিয়ে জোড়া দাও
echo 'Hello' 'World!'

# আউটপুট: Hello World!

# ভেরিয়েবলগুলি জোড়া দাও
set greet "Howdy"
set who "Partner"
echo $greet $who

# আউটপুট: Howdy Partner

# স্পেস ছাড়া জোড়া দাও স্ট্রিং জয়েনের মাধ্যমে
set file "report"
set ext "txt"
string join '' $file '.' $ext

# আউটপুট: report.txt
```

## গভীর ডুব
জোড়া দেওয়া প্রোগ্রামিংয়ের শুরু থেকেই আছে। ফিশে, `string join` পুরোনো পদ্ধতি, যেমন `echo`-এর পরে বিনা উদ্ধৃতিচিহ্নের সাথে স্ট্রিং ভ্যারিয়েবল ব্যবহার, এই পদ্ধতির থেকে পরিষ্কার। এই পদ্ধতি সাবকমান্ড ওভারহেড এড়িয়ে চলে, যা কর্মক্ষমতায় লাভ হতে পারে।

বিকল্পগুলো প্রিন্টএফ-এর মতো, যা আরো বেশি ফরম্যাটিং নিয়ন্ত্রণ প্রদান করে কিন্তু সহজ জোড়া দেওয়ার অপারেশনের জন্য একটু বেশি জটিল। উদাহরণ:

```fish
set firstName "Ada"
set lastName "Lovelace"
printf "%s %s\n" $firstName $lastName
```

ফিশের `string` কমান্ডটি স্ট্রিং ম্যানিপুলেশন টুলবক্সের একটি অংশ যা টেক্সট প্রসেসিংকে আরো সহজ করার জন্য চালু করা হয়েছে। এটা শুধুমাত্র ফিশে অনন্য নয়, কিন্তু এটির একটি অন্তর্নির্মিত টুল হিসেবে অন্তর্ভুক্তি সবকিছু সহজ রাখে।

## আরও দেখুন
- অফিশিয়াল ফিশ ডকুমেন্টেশন: [লিঙ্ক](https://fishshell.com/docs/current/cmds/string.html)
- কমিউনিটি টিউটোরিয়ালস: [লিঙ্ক](https://fishshell.com/docs/current/tutorial.html#tutorial)
- শেলসে স্ট্রিং ম্যানিপুলেশন সম্পর্কিত আলোচনা: [লিঙ্ক](https://unix.stackexchange.com/questions/131766/why-does-my-shell-script-choke-on-whitespace-or-other-special-characters)
