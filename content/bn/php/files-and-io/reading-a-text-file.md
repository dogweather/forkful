---
title:                "টেক্সট ফাইল পড়া"
date:                  2024-03-17T18:09:01.341071-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
PHP-তে একটি টেক্সট ফাইল পড়া মানে আপনার স্ক্রিপ্টে ফাইল থেকে বিষয়বস্তু টেনে আনা। প্রোগ্রামাররা তাদের কোডকে জটিল করে তোলা ছাড়াই ডাটা স্টোরেজ, কনফিগারেশন অথবা বড় ডেটাসেট প্রক্রিয়া করতে এটি করে থাকেন।

## কিভাবে:
### `file_get_contents` ব্যবহার করে:
```PHP
$content = file_get_contents("example.txt");
echo $content;
```
নমুনা আউটপুট:
```
হ্যালো, ওয়ার্ল্ড!
এটি টেক্সট ফাইল থেকে বিষয়বস্তু।
```

### `fopen` এবং `fgets` ব্যবহার করে:
```PHP
$handle = fopen("example.txt", "r");
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        echo $line;
    }
    fclose($handle);
}
```
নমুনা আউটপুট:
```
হ্যালো, ওয়ার্ল্ড!
এটি টেক্সট ফাইল থেকে বিষয়বস্তু।
```

### `file_put_contents` ব্যবহার করে ফাইলে লেখা:
```PHP
$newContent = "নতুন টেক্সট যোগ করুন।";
file_put_contents("example.txt", $newContent);
```

## গভীরে ডাইভ
টেক্সট ফাইল পড়া প্রোগ্রামিং যত পুরানো, তার প্রায় সমান। ডাটাবেজের আগে, কনফিগ ফাইল ও ব্যবহারকারীর ডাটা প্রায়ই সহজ টেক্সট ফাইলে থাকতো। XML এবং JSON ফাইলের মতো বিকল্পগুলো কাঠামোবদ্ধ, পার্স করা সহজ এবং জটিল ডাটা জন্য উপযুক্ত।

PHP-তে, `file_get_contents` এবং `file()` পড়ার জন্য দ্রুত; প্রথমটি সবকিছু একটি স্ট্রিং হিসাবে পেতে, এবং পরেরটি একটি অ্যারেতে। `fopen`-এর সঙ্গে `fgets` বা `fread` ব্যবহার করলে আপনি বেশি নিয়ন্ত্রণ পেতে পারেন, বিশেষ করে বড় ফাইলের ক্ষেত্রে, কারণ আপনি এটি লাইন বাই লাইন অথবা চাঙ্কে পড়তে পারেন।

কিছু নান্দনিকতা: `fopen` উপযুক্ত অনুমতি প্রয়োজন, নাহলে এটি ব্যর্থ হবে; এর ত্রুটিগুলি সম্ভালাটা সেরা অনুশীলন। `file_put_contents` ব্যবহার করার সময় মনে রাখবেন এটি ডিফল্টরুপে ফাইলটি ওভাররাইট করে; বিষয়বস্তু যোগ করার জন্য `FILE_APPEND` ফ্ল্যাগ ব্যবহার করুন।

## আরও দেখুন
- `file_get_contents` সম্পর্কে PHP ম্যানুয়াল: https://www.php.net/manual/en/function.file-get-contents.php
- `fopen` সম্পর্কে PHP ম্যানুয়াল: https://www.php.net/manual/en/function.fopen.php
- `fgets` সম্পর্কে PHP ম্যানুয়াল: https://www.php.net/manual/en/function.fgets.php
- `file_put_contents` সম্পর্কে PHP ম্যানুয়াল: https://www.php.net/manual/en/function.file-put-contents.php
- PHP ফাইল হ্যান্ডলিং সম্পর্কে টিউটোরিয়াল: https://www.w3schools.com/php/php_file.asp
