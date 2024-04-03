---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:39.262577-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AC\u09CD\u09AF\u09BE\u09B6\
  \u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u0995\u09CB\u09A8\u09CB \u09AC\u09BF\
  \u09B2\u09CD\u099F-\u0987\u09A8 \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982 \u09AB\
  \u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995 \u09A8\u09C7\u0987\
  , \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u0986\u09AA\u09A8\u09BF \u09B8\u09BE\u09A7\
  \u09BE\u09B0\u09A3 \u099F\u09C7\u09B8\u09CD\u099F \u09AB\u09BE\u0982\u09B6\u09A8\
  \ \u09B2\u09BF\u0996\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u0986\u09B0\
  \u0993 \u099C\u099F\u09BF\u09B2 \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982\u09DF\
  \u09C7\u09B0 \u099C\u09A8\u09CD\u09AF, \u09A4\u09C3\u09A4\u09C0\u09DF \u09AA\u0995\
  \u09CD\u09B7\u09C7\u09B0 \u099F\u09C1\u09B2\u09B8 \u09AF\u09C7\u09AE\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.230794-06:00'
model: gpt-4-0125-preview
summary: "\u09AC\u09CD\u09AF\u09BE\u09B6\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7\
  \ \u0995\u09CB\u09A8\u09CB \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u099F\u09C7\
  \u09B8\u09CD\u099F\u09BF\u0982 \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\
  \u09B0\u09CD\u0995 \u09A8\u09C7\u0987, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u0986\
  \u09AA\u09A8\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u099F\u09C7\u09B8\u09CD\
  \u099F \u09AB\u09BE\u0982\u09B6\u09A8 \u09B2\u09BF\u0996\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7\u09A8\u0964 \u0986\u09B0\u0993 \u099C\u099F\u09BF\u09B2 \u099F\u09C7\
  \u09B8\u09CD\u099F\u09BF\u0982\u09DF\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF, \u09A4\
  \u09C3\u09A4\u09C0\u09DF \u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u099F\u09C1\u09B2\
  \u09B8 \u09AF\u09C7\u09AE\u09A8 `bats-core` \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\
  \u09DF \u09B9\u09DF\u09C7 \u0989\u09A0\u09C7\u099B\u09C7\u0964\n\n#."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কিভাবে:
ব্যাশের মধ্যে কোনো বিল্ট-ইন টেস্টিং ফ্রেমওয়ার্ক নেই, কিন্তু আপনি সাধারণ টেস্ট ফাংশন লিখতে পারেন। আরও জটিল টেস্টিংয়ের জন্য, তৃতীয় পক্ষের টুলস যেমন `bats-core` জনপ্রিয় হয়ে উঠেছে।

### পিওর ব্যাশে বেসিক টেস্ট উদাহরণ:
```bash
function test_example_function {
  result=$(your_function 'test_input')
  expected_output="expected_output"
  
  if [[ "$result" == "$expected_output" ]]; then
    echo "টেস্ট পাস হয়েছে।"
    return 0
  else
    echo "টেস্ট ব্যর্থ হয়েছে। প্রত্যাশিত '$expected_output', পাওয়া গেছে '$result'"
    return 1
  fi
}

# টেস্ট ফাংশন আহ্বান
test_example_function
```
নমুনা আউটপুট:
```
টেস্ট পাস হয়েছে।
```

### টেস্টিংয়ের জন্য `bats-core` ব্যবহার করা:
প্রথমে, `bats-core` ইনস্টল করুন। এটি সাধারণত আপনার প্যাকেজ ম্যানেজারের মাধ্যমে অথবা এর রেপোজিটরি ক্লোন করে করা যায়।

এরপর, আপনার টেস্টগুলি আলাদা `.bats` ফাইলে লিখুন।

```bash
# ফাইল: example_function.bats

#!/usr/bin/env bats

@test "টেস্ট উদাহরণ ফাংশন" {
  result="$(your_function 'test_input')"
  expected_output="expected_output"
  
  [ "$result" == "$expected_output" ]
}
```
আপনার টেস্টগুলি চালাতে, কেবল `.bats` ফাইলটি এক্সিকিউট করুন:
```bash
bats example_function.bats
```
নমুনা আউটপুট:
```
 ✓ টেস্ট উদাহরণ ফাংশন

1 টেস্ট, 0 ব্যর্থতা
```

এই পদ্ধতি আপনাকে আপনার ডেভেলপমেন্ট ওয়ার্কফ্লোতে সহজেই টেস্টিং একীভূত করতে দেয়, আপনার ব্যাশ স্ক্রিপ্টগুলির বিশ্বাসযোগ্যতা এবং স্থিরতা নিশ্চিত করে।
