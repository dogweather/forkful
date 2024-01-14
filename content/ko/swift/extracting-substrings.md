---
title:    "Swift: 부분 문자열 추출하기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜: 개발자들이 문자열을 추출하는 이유 ##

문자열 추출은 보통 문장에서 원하는 부분만을 따로 뽑아내기 위해 사용됩니다. 예를 들어, 어떤 게임에서 이름을 입력하는 부분이 있을 때, 이름의 일부만을 사용하는 경우가 있습니다. 이때 문자열 추출을 사용하여 원하는 부분만을 뽑아내어 사용할 수 있습니다.

## 추출하는 방법 ##

Swift에서는 `substring` 메소드를 사용하여 문자열을 추출할 수 있습니다. 아래는 코드 예제와 결과를 보여주는 예시입니다.

```Swift
let sentence = "나는 한국어로 블로그를 작성 중입니다."
let extracted = sentence.substring(from: 9)
print(extracted)
```

결과는 다음과 같습니다.

```
"한국어로 블로그를 작성 중입니다."
```

위 예제에서는 `substring` 메소드를 사용하여 9번째 인덱스부터 문자열을 추출하였습니다. 또한 시작 인덱스인 9를 생략하고 `substring(from:)` 형태로도 사용할 수 있습니다. 이렇게 하면 자동으로 첫번째 인덱스부터 추출하게 됩니다.

```Swift
let sentence = "나는 한국어로 블로그를 작성 중입니다."
let extracted = sentence.substring(from: sentence.startIndex)
print(extracted)
```

결과는 다음과 같습니다.

```
"나는 한국어로 블로그를 작성 중입니다."
```

`substring` 메소드를 사용하여 문자열의 일부만을 추출하는 것 이외에도, `prefix`와 `suffix` 메소드를 사용하여 문자열의 시작 부분이나 끝 부분만을 추출할 수도 있습니다. 아래 예제는 `prefix`를 사용하여 처음 4개의 글자만을 추출하는 예시입니다.

```Swift
let sentence = "나는 한국어로 블로그를 작성 중입니다."
let extracted = sentence.prefix(4)
print(extracted)
```

결과는 다음과 같습니다.

```
"나는 "
```

`suffix` 메소드 역시 유사하게 사용할 수 있습니다.

## 깊게 파헤치기 ##

위 예시에서는 하나의 문자열에서 일부분만을 추출하는 방법을 알아보았습니다. 하지만 만약 하나의 문자열에 여러 개의 구분자가 있는 경우 어떻게 추출할 수 있을까요? 이럴 때에는 `split` 메소드를 사용하여 문자열을 배열로 나누고, 배열의 원하는 부분을 추출할 수 있습니다. 아래 예제는 띄어쓰기를 구분자로 사용하여 문자열을 나눈 뒤, 첫번째 단어만 추출하는 예시입니다. 

```Swift
let sentence = "나는 한국어로 블로그를 작성 중입니다."
let words = sentence.split(separator: " ")
let extracted = words[0]
print(extracted)
```

결과는 다음과 같습니다.

```
"나는"
```

이처럼 `substring` 및 관련 메소드를 사용하여 문자열을 추출하는 것은 매우 유용한 방법입니다. 유연하게 원하는 부분만을 추출하고 활용할 수 있으니, 사용법을 꼭 익혀두시기 바랍니다!

# 또 다른 정보들 보기 ##

- [Swift 공식 문서 - String](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift 소스 코드 예제를 활용한 문자열 추출 방법](https://www.hackingwithswift.com/example-code/strings/how-to-skip-prefixes-in-their-entirety)
- [Swift Advanced String Techniques - Jameson Quave](https://medium.com/better-programming/swift