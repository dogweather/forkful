---
date: 2024-01-20 17:54:56.726296-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Ruby\uC5D0\uC11C \uD14D\
  \uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uB294 \uAE30\uBCF8\uC801\uC778 \uBC29\uBC95\
  \uC744 \uC0B4\uD3B4\uBD05\uC2DC\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:56.021192-06:00'
model: gpt-4-1106-preview
summary: "Ruby\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uB294 \uAE30\
  \uBCF8\uC801\uC778 \uBC29\uBC95\uC744 \uC0B4\uD3B4\uBD05\uC2DC\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

## How to: (어떻게 하나요?)
Ruby에서 텍스트 파일을 읽는 기본적인 방법을 살펴봅시다.

```Ruby
# 전체 파일을 한 번에 읽기
contents = File.read('example.txt')
puts contents

# 파일을 줄 단위로 읽기
File.foreach('example.txt') do |line|
  puts line
end
```
Sample output:
```
안녕하세요, Ruby 파일 읽기 예제입니다!

아름다운 한강이 보이네요~
```

## Deep Dive (심층 분석)
예전에는 데이터를 디스크에 직접 쓰고 읽는 저수준 방식을 사용했습니다. Ruby에서는 `IO` 클래스와 `File` 클래스로 추상화를 제공하여 쉽게 파일을 다룰 수 있게 해 줍니다. `File.read`는 파일의 전체 내용을 메모리에 한 번에 적재하는 반면, `File.foreach`는 파일을 한 줄씩 읽으면서 반복 처리합니다. 큰 파일을 다룰 때는 `File.foreach`가 메모리 사용 면에서 더 효율적입니다.

## See Also (추가 정보)
- Ruby 파일 입출력에 대한 더 자세한 정보는 [Ruby-Doc File Class](https://ruby-doc.org/core/File.html)를 참고하세요.
- 더 많은 Ruby 예제와 튜토리얼은 [Ruby Learning](http://rubylearning.com/)에서 찾아볼 수 있습니다.
