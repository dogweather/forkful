---
title:                "কমান্ড লাইন আর্গুমেন্টগুলি পড়া"
date:                  2024-03-17T18:10:08.955670-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
কমান্ড লাইন আর্গুমেন্ট পড়া মানে আপনার স্ক্রিপ্টের নামের পরে টাইপ করা অতিরিক্ত বিটগুলি গ্রহণ করা, যা স্ক্রিপ্টের আচরণ কাস্টমাইজ় করার জন্য সিক্রেট হ্যান্ডশেকের মতো। প্রোগ্রামাররা এটি করে স্ক্রিপ্টগুলিকে নির্বিঘ্নে লচ্ছিত এবং ইন্টার‌্যাক্টিভ করে তুলতে। 

## কি ভাবে:

ধরুন `greet.fish` হলো আপনার স্ক্রিপ্ট। আপনি চান এটি একটি নাম নিতে এবং একটি অভিবাদন দিতে।

```fish
#!/usr/bin/env fish

# আর্গুমেন্টগুলি $argv এ সংরক্ষিত হয়
# $argv[1] প্রথম আর্গুমেন্ট, $argv[2] দ্বিতীয় এবং তা সেইভাবে চলে।

set name $argv[1]
echo "Hello, $name!"
```

চালিয়ে দেখুন:

```shell
$ fish greet.fish World
Hello, World!
```

এখন, একাধিক আর্গুমেন্টের সাথে:

```fish
#!/usr/bin/env fish

# সব আর্গুমেন্টের মধ্য দিয়ে লুপ করুন
for arg in $argv
    echo "Hello, $arg!"
end
```

চেষ্টা করুন:

```shell
$ fish greet.fish Earth Mars Venus
Hello, Earth!
Hello, Mars!
Hello, Venus!
```

ফ্ল্যাগস হ্যান্ডল করতে (যেমন `-u` আপারকেসের জন্য):

```fish
#!/usr/bin/env fish

# "-u" আর্গুমেন্টের জন্য চেক করুন
set -l uppercase_mode off
for arg in $argv
    if test "$arg" = "-u"
        set uppercase_mode on
    else if set -q uppercase_mode[1]; and string match --quiet -- "$uppercase_mode" "on"
        echo (string upper "$arg")
    else
        echo $arg
    end
end
```

এবং সক্রিয় করুন:

```shell
$ fish greet.fish -u mercury venus
MERCURY
VENUS
```

## গভীর ডুব

Fish Shell দীর্ঘকাল ধরে কমান্ড লাইন আর্গুমেন্টের মাস্টারী রাখে, অন্যান্য শেলগুলির মতোই। যা Fish কে আলাদা করে তোলে তা হল এর সরল ডিজাইন। নেই `$1, $2... $n` মনে রাখার ঝামেলা; এটি একটি এরে `$argv`, যদি আপনি অন্যান্য প্রোগ্রামিং ভাষায় পরিচিত থাকেন তবে পরিচিত টেরিটরি।

এর বিকল্পগুলি রয়েছে, যেমন bash, zsh, ইত্যাদি, কিন্তু Fish's স্ক্রিপ্টিং সিনট্যাক্স আরও পড়াযোগ্য এবং সরল হওয়ার লক্ষ্যে। পারম্পরিক `shift` কমান্ড বা সমস্ত আর্গুমেন্টের জন্য `$@` নিয়ে ডিল করার পরিবর্তে, Fish এর রয়েছে বন্ধুত্বপূর্ণ `$argv` এবং সুন্দর স্ক্রিপ্টিং কন্সট্রাক্টস যেমন `for` লুপ এবং `if` শর্তাবলী যা ক্রিপ্টিক প্রতীকের চেয়ে স্পষ্ট শব্দে বেশি।

বাস্তবায়ন করার সময়, এটি জরুরি ভাবে বিবেচনা করা হয় যে আপনার স্ক্রিপ্ট কিভাবে ব্যবহার করা হবে। এতে কি ডিফল্ট মানগুলি প্রয়োজন হবে? ব্যবহারকারীরা কি ইনপুট দেওয়ার কি যেনে থাকবেন? নিশ্চিত করুন আপনি এমন কেসগুলি হ্যান্ডল করেন যেখানে ব্যবহারকারীরা আর্গুমেন্ট পাস করতে ভুলে যান বা ভুল অর্ডারে পাস করেন।

## দেখুন এছাড়াও

- কমান্ড লাইন আর্গুমেন্ট সম্পর্কিত অফিশিয়াল ফিশ ডকুমেন্টেশন: [fishshell.com/docs/current/#syntax-command-line](https://fishshell.com/docs/current/#syntax-command-line)
- উন্নত স্ক্রিপ্টিং এবং ফিশে নিজস্ব ফাংশন তৈরি করার জন্য: [fishshell.com/docs/current/#defining-functions](https://fishshell.com/docs/current/#defining-functions)
- অন্যান্য শেলগুলির পটভূমিতে থাকা ব্যবহারকারীদের জন্য ফিশ পরিচিতি: [fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
