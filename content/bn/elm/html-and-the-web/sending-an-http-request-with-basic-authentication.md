---
title:                "বেসিক অথেন্টিকেশন সহ HTTP রিকুয়েস্ট প্রেরণ"
date:                  2024-03-17T18:19:01.141409-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

বেসিক অথেনটিকেশন সহ HTTP অনুরোধ প্রেরণ করা হলো লগইন পরিচয়পত্র (ব্যবহারকারীর নাম এবং পাসওয়ার্ড) অনুরোধের শিরোনামে যুক্ত করে সুরক্ষিত সম্পদে অ্যাক্সেস অর্জন করা। প্রোগ্রামাররা এটি সেই সব HTTP API-তে সহজ অথেনটিকেশনের জন্য ব্যবহার করে থাকেন যেখানে আরও জটিল পদ্ধতির অতিরিক্ত বোঝা প্রয়োজন নেই।

## কিভাবে:

`Http` প্যাকেজ ব্যবহার করে Elm HTTP অনুরোধ পাঠায়। বেসিক অথ্ যোগ করতে, আপনি পরিচয়পত্রগুলোকে এনকোড করে এবং অনুরোধের শিরোনামগুলোতে সংযুক্ত করেন।

```Elm
import Http
import Base64

type alias Model = { ... }
type Msg = HttpRequestCompleted (Result Http.Error String)

-- ব্যবহারকারীর নাম এবং পাসওয়ার্ড এনকোড করুন
basicAuthHeader : String -> String -> Http.Header
basicAuthHeader username password =
    let
        credentials = username ++ ":" ++ password
        encodedCredentials = Base64.encode credentials
    in
    Http.header "Authorization" ("Basic " ++ encodedCredentials)

-- HTTP অনুরোধ পাঠান
sendRequestWithBasicAuth : Cmd Msg
sendRequestWithBasicAuth =
    let
        url = "https://example.com/protected/resource"
        request =
            Http.request
                { method = "GET"
                , headers = [ basicAuthHeader "myUsername" "myPassword" ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectString (HttpRequestCompleted)
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Http.send HttpRequestCompleted request
```

উপরের ফাংশান ডাকা হলে, Elm নির্দিষ্ট URL-এ এনকোডেড ব্যবহারকারীর নাম এবং পাসওয়ার্ড সেট করা অনুমতি শিরোনামের সাথে GET অনুরোধ সম্পাদন করবে।

## গভীর তলাশ

HTTP অনুরোধের প্রতি Elm এর আসলেই ভাষার সার্বিক দর্শনের প্রতিফলন: নিরাপদ, রক্ষণাবেক্ষণ সহজ, এবং বোঝার যোগ্য। `Http` প্যাকেজটি Elm আর্কিটেকচারের সাথে অনুরোধগুলি পরিপূরক করে।

বেসিক অথেনটিকেশন হলো ওয়েবের প্রাচীনতম অংশ, মৌলিক HTTP স্পেসিফিকেশনের (RFC 7617) অংশ। এটি সরল তবে খুব নিরাপদ নয় কারণ পরিচয়পত্রগুলো শুধুমাত্র বেস64 এনকোড করা হয়েছে, এনক্রিপ্ট করা হয়নি। তাই, ট্রান্সমিটেশন এনকোড করতে HTTPS ব্যবহার করা জরুরি।

বেসিক অথেনটিকেশনের বিকল্প অন্তর্ভুক্ত হলো OAuth, টোকেনের মতো JWT, অথবা API কী, প্রতিটি বাড়তি জটিলতা এবং উন্নতি করা নিরাপত্তা নিয়ে আসে। Elm এই পদ্ধতিগুলি সমর্থন করে তবে প্রায়শই অতিরিক্ত প্যাকেজ বা কাস্টম এনকোডার এবং ডিকোডার প্রয়োজন হয়।

## দেখুন ও

- Elm এর অফিসিয়াল `Http` প্যাকেজ ডকুমেন্টেশন: [package.elm-lang.org/packages/elm/http/latest](https://package.elm-lang.org/packages/elm/http/latest)
- Elm এর `Base64` প্যাকেজ সোর্স: [package.elm-lang.org/packages/truqu/elm-base64/latest](https://package.elm-lang.org/packages/truqu/elm-base64/latest)
- RFC 7617, 'বেসিক' HTTP অথেনটিকেশন স্কিম: [tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
