---
title:                "Swift: Json 처리하기"
simple_title:         "Json 처리하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/working-with-json.md"
---

{{< edit_this_page >}}

Swift 프로그래밍을 배우는 사람들에게 중요한 부분 하나는 JSON 데이터를 다루는 법입니다. JSON은 우리가 일상적으로 접하는 데이터 형식이며, 많은 앱과 웹서비스들이 이 형식을 사용합니다. 그래서 JSON 데이터를 다루는 방법을 잘 알아두는 것이 중요합니다.

## 왜

왜 JSON 데이터를 다루는 것이 중요한지 궁금하신가요? 이는 우리가 일상에서 사용하는 많은 앱과 웹서비스들이 JSON 데이터를 사용하기 때문입니다. 예를 들어, 우리가 페이스북에서 새로운 게시물을 가져오거나, 인스타그램에서 새로운 사진을 다운로드 받을 때, JSON 데이터가 사용됩니다.

## 다루는 방법

이제 우리는 Swift를 사용하여 JSON 데이터를 다루는 방법에 대해 알아보겠습니다. 먼저 데이터를 가져오기 위해 필요한 URL을 정의해야 합니다.

````Swift
let url = URL(string: "https://example.com/api/posts")
````

다음은 `URLSession` 객체를 사용하여 데이터를 가져오는 방법입니다.

````Swift
let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
    if let data = data {
        // JSON 데이터를 다루는 코드 작성
    }
}
task.resume()
````

이제 우리는 다운로드한 데이터를 `JSONSerialization`을 사용하여 파싱해야 합니다. 이를 위해 `try-catch` 문을 사용해야 하며, 다음과 같은 코드를 사용합니다.

````Swift
do {
    let jsonObject = try JSONSerialization.jsonObject(with: data, options: .allowFragments)
    // 파싱한 JSON 데이터를 사용하는 코드 작성
} catch {
    print("JSON parsing error: \(error)")
}
````

위의 예제 코드에서 우리는 다운로드한 데이터를 `try-catch` 문을 사용하여 파싱합니다. 만약 파싱 도중 에러가 발생하면 `JSON parsing error`가 출력됩니다.

## 깊이 있는 분석

JSON 데이터는 일반적으로 `key-value` 형식을 가져야 합니다. 즉, 데이터는 `key`와 `value`의 쌍으로 이루어져 있으며, 이를 구분하기 위해 중괄호 `{}`가 사용됩니다. 예를 들어, 페이스북 게시물의 JSON 데이터는 다음과 같이 구성됩니다.

````Swift
{
    "author": "John Doe",
    "text": "Hello, world!",
    "likes": 10,
    "comments": [
        "Great post!",
        "Thanks for sharing!"
    ]
}
````

위의 예제에서 보다시피 하나의 `key`에 여러 개의 `value`를 가질 수 있습니다. 또한 `key`와 `value`의 타입은 다양하게 설정할 수 있으며, 우리는 이를 활용하여 다양한 데이터를 다룰 수 있습니다.

## 참고 자료

JSON 데이터를 다루는 방법에 대해 더 알아보려면 아래의 링크를 참조해주세요.

- [API with Swift: Working with JSON](https://www.raywenderlich.com/3437208-api-with-swift-working-with-json)
- [SwiftyJSON](https://github.com/SwiftyJSON/SwiftyJSON)
- [How to Use JSON in Swift](https://www.codementor.io/@danielgalasko/how-to-use-json-files-in-swift-4-xvi1mp2s1)
- [JSON in Swift: Getting Started](https://medium.com/ios-os-x-development/json-in-swift-tutorial-63914b3da963)

## 관련 자료

우리는 이제 Swift를 사용하여 JSON 데이터를 다루는 방법에