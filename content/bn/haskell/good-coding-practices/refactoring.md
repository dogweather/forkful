---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:11:55.038516-06:00
description: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\
  \ \u09B9\u09B2 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u0995\u09C7 \u09AA\
  \u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u099B\u09BE\u09A1\u09BC\u09BE\u0987\
  \ \u0993 \u09AC\u09B9\u09BF\u09B0\u09BE\u0997\u09A4 \u0986\u099A\u09B0\u09A3\u09C7\
  \ \u0995\u09CB\u09A8\u09CB \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u09A8\
  \u09BE \u0995\u09B0\u09C7 \u09A4\u09BE \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\
  \u09B0 \u0993 \u09B8\u09BE\u099C\u09BE\u09A8\u09CB\u09B0 \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0964 \u098F\u099F\u09BF \u09AE\u09C2\u09B2\
  \u09A4 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u0995\u09C7 \u09AA\u09A1\
  \u09BC\u09BE,\u2026"
lastmod: '2024-03-17T18:47:44.092794-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\
  \ \u09B9\u09B2 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u0995\u09C7 \u09AA\
  \u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u099B\u09BE\u09A1\u09BC\u09BE\u0987\
  \ \u0993 \u09AC\u09B9\u09BF\u09B0\u09BE\u0997\u09A4 \u0986\u099A\u09B0\u09A3\u09C7\
  \ \u0995\u09CB\u09A8\u09CB \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u09A8\
  \u09BE \u0995\u09B0\u09C7 \u09A4\u09BE \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\
  \u09B0 \u0993 \u09B8\u09BE\u099C\u09BE\u09A8\u09CB\u09B0 \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0964 \u098F\u099F\u09BF \u09AE\u09C2\u09B2\
  \u09A4 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u0995\u09C7 \u09AA\u09A1\
  \u09BC\u09BE,\u2026"
title: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982"
---

{{< edit_this_page >}}

## কি এবং কেন?
রিফ্যাক্টরিং হল আপনার কোডকে পরিবর্তন ছাড়াই ও বহিরাগত আচরণে কোনো পরিবর্তন না করে তা পরিষ্কার ও সাজানোর প্রক্রিয়া। এটি মূলত আপনার কোডকে পড়া, রক্ষণাবেক্ষণ, এবং বর্ধন করা সহজ করতে তা পরিষ্কার এবং সাজানোর ব্যাপারে। এটি বাগ দূরীকরণ এবং কর্মক্ষমতা উন্নতির ক্ষেত্রেও সহায়ক হতে পারে।

## কিভাবে:
ধরুন, আপনার কাছে Haskell কোডের একটি খণ্ড আছে যা আপনার প্রিয় গানের চেয়ে বেশি নিজেকে পুনরাবৃত্তি করছে। এখানে আপনি কিভাবে ফাংশন ব্যবহার করে তা রিফ্যাক্টর করতে পারেন তার একটি দ্রুত দৃষ্টান্ত দেওয়া হলো।

রিফ্যাক্টরিং করার আগে:

```haskell
printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  putStrLn $ "Customer: " ++ customer
  putStrLn $ "Total: " ++ show total
  putStrLn $ "Item: " ++ item
```

একটু রিফ্যাক্টরিং করার পর:

```haskell
printDetail :: String -> String -> IO ()
printDetail label value = putStrLn $ label ++ ": " ++ value

printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  printDetail "Customer" customer
  printDetail "Total" (show total)
  printDetail "Item" item

-- নমুনা আউটপুট:
-- Customer: Alice
-- Total: $42.00
-- Item: Haskell Programming Guide
```

যেমন আপনি দেখতে পাচ্ছেন, পৃথক একটি `printDetail` ফাংশনে সাধারণ প্যাটার্নটি এক্সট্র্যাক্ট করে আমরা পুনরাবৃত্তি এড়াতে এবং `printInvoice` কে আরও স্পষ্ট এবং পরিচালনা করা সহজ করতে পারি।

## গভীরে ডুব
যখন Haskell শেষ ৮০-এ দৃশ্যপটে এসেছিল, তখন এটি স্পষ্ট ছিল যে ফাংশনাল প্যারাডাইম কোডিং প্রথায় কিছু নতুন বাতাস নিয়ে আসতে পারে। সময় এগিয়ে, Haskell এ রিফ্যাক্টরিং বিশেষ করে সুন্দর হয়ে উঠেছে ধন্যবাদ ফাংশনগুলোকে প্রথম শ্রেণীর নাগরিক হিসেবে এবং এর শক্তিশালী স্থায়ী টাইপ সিস্টেমের কারণে। আপনি আপনার অ্যাপ্লিকেশন ভেঙে ফেলার ভয় ছাড়াই রিফ্যাক্টর করতে পারেন কারণ কম্পাইলার আপনার পিছনে আছে।

ম্যানুয়াল রিফ্যাক্টরিং ছাড়াও, অটোমেটেড টুলস ব্যবহার করা শিক্ষণীয় হলেও, Haskell এর ফাংশনাল প্রকৃতি এবং টাইপ সুরক্ষা এটিকে অন্যান্য ভাষার তুলনায় কম প্রচলিত করে তুলেছে। বাস্তবায়নের দিক থেকে, Haskell এর বৈশিষ্ট্যগুলি যেমন উচ্চ-অর্ডার ফাংশনগুলি, পবিত্রতা, এবং অপরিবর্তনীয়তা ব্যবহার করে রিফ্যাক্টরিং কে আরও মসৃণ করা গুরুত্বপূর্ণ।

"ফাংশন এক্সট্র্যাক্ট করা" মতো রিফ্যাক্টরিংগুলি সাধারণ, তবে আপনি "ইনলাইন ফাংশন", "ভেরিয়েবল পুনঃনামকরণ", এবং "ফাংশন সিগনেচার পরিবর্তন" করার মতো কাজও টাইপ সিস্টেমের ধন্যবাদে আত্মবিশ্বাসের সাথে করতে পারেন। Haskell এর শক্তিশালী টাইপ আনুমান কখনও কখনও অন্যান্য ভাষায় যেগুলি এড়িয়ে যেতে পারে তেমন ত্রুটিগুলি ধরতে পারে।

## আরও দেখুন
Haskell এ রিফ্যাক্টরিং নিয়ে গভীরে ডুব দিতে, Martin Fowler এর "Refactoring: Improving the Design of Existing Code" বইটি ঘাটতে পারেন যেখানে ধারণাগুলি সর্বজনীনভাবে প্রযোজ্য। আপনার Haskell কোড উন্নতির জন্য অটোমেটেড হিন্ট পাওয়ার জন্য hlint টুল চেক করুন। তাছাড়া, সম্প্রদায় অন্তর্দৃষ্টি এবং আরও পাঠের জন্য Haskell উইকি (https://wiki.haskell.org/Refactoring) ভিজিট করুন।
