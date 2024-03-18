---
title:                "বর্তমান তারিখ পেতে"
date:                  2024-03-17T17:48:38.639923-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
Elm-এ বর্তমান তারিখ পেতে মানে সিস্টেম থেকে বর্তমান ক্যালেন্ডার তারিখ আনা। আমরা ঘটনাবলীর সময় চিহ্নিত করা, কার্যক্রম নির্ধারণ, অথবা সময়ের দৈর্ঘ্য ট্র্যাক করার জন্য এটি করি।

## কিভাবে:
Elm তারিখগুলোকে `Time` মডিউলের মাধ্যমে নিয়ন্ত্রণ করে। আপনি বর্তমান সময়কে POSIX টাইমস্ট্যাম্প হিসেবে পাবেন, এরপর তারিখে রূপান্তর করবেন।

```Elm
import Browser
import Task
import Time

type Msg = GetCurrentTime Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetCurrentTime posixTime ->
            let
                -- POSIX সময়কে তারিখ রেকর্ডে রূপান্তর করুন
                date = Time.toDate posixTime
            in
            -- আপনার মডেলকে যথাযথভাবে আপডেট করুন এখানে
            ({ model | date = date }, Cmd.none)

-- বর্তমান সময় পাওয়ার উদ্যোগ নেওয়া
getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform GetCurrentTime Time.now

-- উদাহরণ আউটপুট:
-- date { year = 2023, month = Mar, day = 26 }
```

## গভীর ডুব
পুরানো ওয়েব ভাষায়, তারিখ পাওয়া এক লাইনের কোডের মতো সহজ। Elm ভিন্ন। এটি Elm আর্কিটেকচারের মাধ্যমে বর্তমান সময় প্রাপ্তির মতো পার্শ্ব-প্রভাবগুলিকে স্পষ্ট করে তোলে। এটি কোডের পবিত্রতা এবং রক্ষণাবেক্ষণকে উত্সাহিত করে।

বিকল্পগুলি তৃতীয় পক্ষের প্যাকেজ ব্যবহার করা বা আপনার সার্ভার কোডে তারিখগুলো হ্যান্ডেল করে Elm-এ ফ্ল্যাগ বা পোর্টগুলির মাধ্যমে পাস করা অন্তর্ভুক্ত।

ইমপ্লিমেন্টেশনের দিক থেকে, Elm-এর `Time.now` POSIX টাইমস্ট্যাম্প হিসেবে সময় পায় (Unix epoch থেকে মিলিসেকেন্ড)। এটি সময়ক্ষেত্র-নিরপেক্ষ, এবং আপনি এটিকে `Time` মডিউল থেকে ফাংশন ব্যবহার করে প্রয়োজনমতো ফর্ম্যাট করতে পারেন।

## দেখুনও
- [Elm Time ডকুমেন্টেশন](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm-এর কমান্ড ও সাবস্ক্রিপশনের গাইড](https://guide.elm-lang.org/effects/)
