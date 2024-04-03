---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:28.417535-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: `string` \u0995\u09AE\u09BE\u09A8\
  \u09CD\u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7, \u099F\
  \u09C7\u0995\u09CD\u09B8\u099F\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0\
  \ \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\
  \u09B0\u09BE \u09B8\u09CB\u099C\u09BE\u0964 \u09B6\u09C1\u09A7\u09C1 \u0995\u09B0\
  \u09C1\u09A8."
lastmod: '2024-03-17T18:47:44.482606-06:00'
model: gpt-4-0125-preview
summary: "`string` \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7, \u099F\u09C7\u0995\u09CD\u09B8\u099F\u0995\u09C7\
  \ \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\
  \u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u09B8\u09CB\u099C\u09BE\u0964\
  \ \u09B6\u09C1\u09A7\u09C1 \u0995\u09B0\u09C1\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
`string` কমান্ড ব্যবহার করে, টেক্সটকে লোয়ার কেসে রূপান্তর করা সোজা। শুধু করুন:

```Fish Shell
echo "MAKE ME LOWERCASE" | string lower
```

নমুনা আউটপুট:

```
make me lowercase
```

একটি ভেরিয়েবলের জন্য:

```Fish Shell
set my_string "SHOUTY CASE TEXT"
string lower -q -- $my_string
```

আউটপুট:

```
shouty case text
```

## গভীর অধ্যয়ন:
Fish Shell-এর আগে, Unix ব্যবহারকারীরা প্রায়ই `tr '[:upper:]' '[:lower:]'` অথবা `awk '{print tolower($0)}'` ব্যবহার করতেন। যদিও এগুলি কাজ করে, তবে এগুলি Fish-এর অন্তর্নির্মিত `string lower` ফাংশনের মত পরিষ্কার বা সরল নয়।

Fish `string`-কে v2.3.0 (মে 2016) এ চালু করেছিল, যা স্ট্রিং ম্যানিপুলেশনকে শেলের একটি মূল অংশ হিসেবে উন্নীত করেছিল, বাহ্যিক কমান্ডগুলির প্রয়োজন ছাড়াই। এটি কেস রূপান্তরের মত সাধারণ কাজে সরলতা এবং গতি যোগ করে।

কেন শুধু `tr` অথবা `awk` ব্যবহার না করা? `string lower` হল Fish-এ অন্তর্ভুক্ত, অর্থাৎ এটি দ্রুততর (নতুন প্রসেস শুরু না করে) এবং বিভিন্ন সিস্টেমে স্থির এবং পূর্বাভাসযোগ্য উপায়ে কাজ করে। এটি অন্যান্য স্ট্রিং অপারেশনগুলি পরিচালনা করে এমন বৃহত্তর `string` কমান্ড সুইটেরও অংশ, যা স্ক্রিপ্ট লেখাকে আরও পরিচ্ছন্ন এবং দক্ষ করতে পারে।

## আরও দেখুন:
- `string`-এর অফিসিয়াল ডকুমেন্টেশন: https://fishshell.com/docs/current/cmds/string.html
- Fish Shell GitHub রিপোজিটরি: https://github.com/fish-shell/fish-shell
- `string` এবং ঐতিহাসিক Unix কমান্ডগুলির ঐতিহাসিক প্রেক্ষাপট এবং তুলনা: https://github.com/fish-shell/fish-shell/issues/159
