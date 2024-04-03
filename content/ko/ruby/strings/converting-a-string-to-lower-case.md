---
date: 2024-01-20 17:39:12.986139-07:00
description: "How to: (\uBC29\uBC95) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.973063-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

## How to: (방법)
```Ruby
# downcase 메소드 사용
sentence = "Hello, Ruby World!"
lowercase_sentence = sentence.downcase

puts lowercase_sentence
# 출력: hello, ruby world!
```

```Ruby
# Unicode 문자열에 대해서도 적용
korean_sentence = "안녕하세요, 루비!"
lowercase_korean_sentence = korean_sentence.downcase

puts lowercase_korean_sentence
# 출력: 안녕하세요, 루비!
```

## Deep Dive (심층 분석)
Ruby에서 `downcase` 메소드는 ASCII 문자만 아닌 Unicode 문자에 대해서도 소문자 변환을 지원합니다. 이 기능은 Ruby 2.4부터 향상되었습니다. `downcase` 이외에도 `downcase!` 메소드가 있는데, 이것은 원본 스트링 자체를 변경합니다. 만약 변환 대상이 ASCII 문자가 아니라면, 크게 관심을 끌지는 않지만 `String#unicode_normalize`와 결합하여 사용하기도 합니다.

다른 방법으로 `tr` 메소드를 사용하여 변환할 수도 있습니다만, 이는 주로 특정 문자들만 대상으로 사용되므로 전체 대소문자 변환에는 `downcase`가 훨씬 더 편리합니다.

```Ruby
# tr 메소드 예시
sentence = "Hello World!"
lowercase_sentence = sentence.tr('A-Z', 'a-z')

puts lowercase_sentence
# 출력: hello world!
```

내부적으로 `downcase` 메소드는 C로 작성된 Ruby의 내장 라이브러리에 의해 구현되어 있습니다. 성능 최적화를 위해, Ruby의 많은 문자열 메소드들은 네이티브 코드로 작성되어 있기 때문에 매우 빠릅니다.

## See Also (참고 자료)
- Ruby 문서의 `downcase` 메소드: [https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase)
- Ruby 문서의 `downcase!` 메소드: [https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase-21](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase-21)
- `tr` 메소드에 대해 더 알아보기: [https://ruby-doc.org/core-2.7.0/String.html#method-i-tr](https://ruby-doc.org/core-2.7.0/String.html#method-i-tr)
