---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:08.866484-06:00
description: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F\
  \ \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u0995\u09BF\
  \ \u0998\u099F\u099B\u09C7 \u09A4\u09BE \u09AC\u09C1\u099D\u09A4\u09C7 \u09B8\u09BE\
  \u09B9\u09BE\u09AF\u09CD\u09AF \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u0985\u09A4\u09BF\u09B0\u09BF\u0995\u09CD\u09A4 \u09A4\u09A5\u09CD\u09AF \u09AA\
  \u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\
  \u09B0\u09C7 \u09A5\u09BE\u0995\u09C7 \u09AC\u09BE\u0997 \u09B8\u09A8\u09BE\u0995\
  \u09CD\u09A4 \u0995\u09B0\u09C7 \u098F\u09AC\u0982 \u09B8\u09B9\u099C\u09C7\u0987\
  \u2026"
lastmod: '2024-03-17T18:47:44.500474-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u0995\u09BF \u0998\
  \u099F\u099B\u09C7 \u09A4\u09BE \u09AC\u09C1\u099D\u09A4\u09C7 \u09B8\u09BE\u09B9\
  \u09BE\u09AF\u09CD\u09AF \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\
  \u09A4\u09BF\u09B0\u09BF\u0995\u09CD\u09A4 \u09A4\u09A5\u09CD\u09AF \u09AA\u09CD\
  \u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\
  \u09C7 \u09A5\u09BE\u0995\u09C7 \u09AC\u09BE\u0997 \u09B8\u09A8\u09BE\u0995\u09CD\
  \u09A4 \u0995\u09B0\u09C7 \u098F\u09AC\u0982 \u09B8\u09B9\u099C\u09C7\u0987\u2026"
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
ডিবাগ আউটপুট প্রিন্ট করা মানে আপনার কোডের কি ঘটছে তা বুঝতে সাহায্য করার জন্য অতিরিক্ত তথ্য প্রদান করা। প্রোগ্রামাররা এটি করে থাকে বাগ সনাক্ত করে এবং সহজেই মেরামত করার জন্য।

## কিভাবে:
`echo` এর সঙ্গে স্বাচ্ছন্দ্য পেতে হবে - Fish এ আউটপুটের জন্য Swiss Army ছুরি। এখানে আপনার শেল স্ক্রিপ্টগুলিতে কিছু ডিবাগ প্রিন্ট কিভাবে যোগ করবেন তার উপায়।

```Fish Shell
function greet
    set name $argv[1]
    echo "Hey, $name! Let's debug."
    echo "Running the greet function" >&2
end

greet "Ada"
```
নমুনা আউটপুট:
```
Hey, Ada! Let's debug.
Running the greet function
```
স্ট্যান্ডার্ড আউট (`stdout`) হল আপনার স্ক্রিপ্টের মূল মঞ্চ, কিন্তু ডিবাগ গল্পের জন্য স্ট্যান্ডার্ড এরর (`stderr`) ব্যবহার করুন `>&2` এর সাথে।

## গভীরে ডুব দেওয়া
যখন মনিটরগুলি যতটা গভীর ছিল ততটাই প্রশস্ত, আউটপুট অমূল্য ছিল। স্ট্যান্ডার্ড আউট (`stdout`) খাঁটি, ব্যবহারকারী-মুখি চ্যানেল হয়ে উঠেছিল, যেখানে স্ট্যান্ডার্ড এরর (`stderr`) প্রোগ্রামার-শুধুমাত্র গসিপের জন্য ব্যাক-এলি হয়ে উঠেছিল যেমন ডিবাগ তথ্য।

Fish এ, আউটপুটের জন্য প্রমাণ্য কমান্ডগুলি হল `echo`, `printf`, এবং `print`। `echo` সোজা সাপটা এবং মূলত সাধারণ বার্তা এবং ইনলাইন ডিবাগের জন্য ব্যবহৃত।

যদিও, আপনি শুধুমাত্র `echo` এর সাথে আটকে নেই। ফরম্যাট করা স্ট্রিংয়ের জন্য `printf` পছন্দ করুন, অথবা পুনঃনির্দেশন (`>` বা `>>`) ব্যবহার করে পরে পর্যালোচনার জন্য ফাইলে ডিবাগ তথ্য ডাম্প করুন।

বাস্তবায়নের জন্য, `stderr` ব্যবহার করা ইউনিক্স বিশ্ব থেকে একটি ধারণা, যা আসল আউটপুট (গম) থেকে ডিবাগ শব্দ (ভূসি) পৃথক করতে সাহায্য করে। এর অর্থ হল ব্যবহারকারীরা আপনার স্ক্রিপ্টের আসল আউটপুটটি এখনও পাইপ করতে পারে ডিবাগ গর্বিত ছাড়াই।

## আরও দেখুন
- Fish Shell ডকুমেন্টেশনের [Commands](https://fishshell.com/docs/current/commands.html) উপর
- StackOverflow: [Fish এ ডিবাগিং](https://stackoverflow.com/questions/tagged/fish) সম্পর্কিত আলোচনা এবং উদাহরণ
- Greg's Wiki: [I/O পুনঃনির্দেশন](https://mywiki.wooledge.org/BashGuide/InputAndOutput#Redirection) সম্পর্কে গভীর তথ্য
