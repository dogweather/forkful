---
title:                "HTTP অনুরোধ প্রেরণ করা"
date:                  2024-03-17T18:17:59.675913-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

HTTP request পাঠানো মানে ওয়েব সার্ভারকে কিছু করতে বা আপনাকে কিছু দিতে অনুরোধ করা। প্রোগ্রামাররা ওয়েব সার্ভিসের সাথে মিথস্ক্রিয়া করতে, ডাটা আনতে, ফর্ম জমা দিতে বা API এর সাথে যোগাযোগ করতে এটি করে থাকেন।

## কিভাবে:

Kotlin এ HTTP অনুরোধ পাঠানো সরল। এখানে `khttp`, একটি বন্ধুত্বপূর্ণ লাইব্রেরী ব্যবহার করে একটি মৌলিক উদাহরণ দেয়া হল:

```Kotlin
import khttp.get

fun main() {
    val response = get("https://api.github.com/users/octocat/orgs")
    println(response.text)
}
```

আউটপুট:

```Kotlin
[{"login":"octo-org","id":583231,"url":"https://api.github.com/orgs/octo-org", ...}]
```

আরও দৃঢ় প্রয়োজনের জন্য, এখানে আসিঙ্ক্রোনাসভাবে ডাটা আনার জন্য `ktor`, একটি Kotlin ফ্রেমওয়ার্ক ব্যবহার করে একটি অংশ দেয়া হল:

```Kotlin
import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient(CIO)
    val response: String = client.get("https://api.github.com/users/octocat/orgs")
    println(response)
    client.close()
}
```

আউটপুট প্রথম উদাহরণের মতই।

## গভীর ডুব

`khttp` লাইব্রেরী Python-এর `requests`-এর নকশা অনুসরণ করে একটি সুবিধাজনক টুল, যা দ্রুত স্ক্রিপ্ট লেখার জন্য দুর্দান্ত কিন্তু সেটি সক্রিয়ভাবে রক্ষণাবেক্ষণ করা হয়নি। `ktor` হল JetBrains দ্বারা ডিজাইন করা একটি নতুন, সক্রিয় প্রকল্প, যা আসিঙ্ক্রোনাস অপারেশনগুলোর জন্য করুটিনস দিয়ে তৈরি। এটি স্কেলযোগ্য অ্যাপ্লিকেশনের জন্য মানানসই। উভয়ই HTTP অনুরোধ সামলায় কিন্তু বিভিন্ন ব্যবহারের ক্ষেত্রে পরিবেশন করে।

ঐতিহাসিকভাবে, Kotlin-এ HTTP অনুরোধগুলি `HttpURLConnection` বা Apache-এর `HttpClient` এর মতো Java লাইব্রেরিগুলি দিয়ে সম্পাদন করা হত, যা এখনো বৈধ কিন্তু আরও বাগবিস্তৃত এবং Kotlin-এর ভাষাগত বৈশিষ্ট্যের অভাব।

বাস্তবায়নের ক্ষেত্রে, মনে রাখবেন সাধারণ HTTP ত্রুটিগুলি এবং প্রতিক্রিয়া কোড পড়তে হবে। আপনাকে `try-catch` ব্যবহার করতে হবে নেটওয়ার্ক ব্যতিক্রমী পরিস্থিতির জন্য এবং সম্ভবত শিরোনাম এবং প্রশ্ন প্যারামিটারের সাথে কাজ করতে হবে।

## দেখুন ও

- Ktor ডকুমেন্টেশন: https://ktor.io/
- khttp GitHub রিপোজিটরি: https://github.com/jkcclemens/khttp (রক্ষণাবেক্ষণের অবস্থা লক্ষ্য করুন)
- Kotlin HTTP কল সহ HttpURLConnection: https://kotlinlang.org/api/latest/jvm/stdlib/java.net/-http-u-r-l-connection/
