---
date: 2024-01-20 17:56:53.380860-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Ruby\uC5D0\uC11C \uBA85\uB839 \uC904 \uC778\
  \uC218\uB97C \uC77D\uAE30 \uC704\uD55C \uC608\uC2DC \uCF54\uB4DC\uC640 \uACB0\uACFC\
  \uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:56.018407-06:00'
model: gpt-4-1106-preview
summary: "Ruby\uC5D0\uC11C \uBA85\uB839 \uC904 \uC778\uC218\uB97C \uC77D\uAE30 \uC704\
  \uD55C \uC608\uC2DC \uCF54\uB4DC\uC640 \uACB0\uACFC\uC785\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

## 사용 방법:
Ruby에서 명령 줄 인수를 읽기 위한 예시 코드와 결과입니다.

```Ruby
# example.rb
puts ARGV
```

커맨드 라인에서:

```sh
ruby example.rb 이것은 테스트입니다
```

출력:

```
이것은
테스트입니다
```

## 심층 분석:
명령 줄 인수를 읽는 법은 오래전부터 있었습니다. Unix 시스템과 C언어에서 간단히 사용되기 시작했죠. `ARGV`는 Ruby에 내장된 글로벌 배열로, 주어진 인수들을 문자열로 포함하고 있습니다. 대안으로는 `OptionParser`라는 Ruby 표준 라이브러리가 있어 복잡한 명령 줄 인터페이스를 쉽게 처리할 수 있습니다. `ENV`라는 환경 변수를 통해 OS 레벨의 설정에 접근하는 것도 유용합니다.

## 참고자료:
- [Ruby-Doc: ARGV](https://ruby-doc.org/core-2.7.0/ARGF.html)
- [Ruby-Doc: OptionParser](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html)
