---
date: 2024-01-20 17:56:53.380860-07:00
description: "\uBA85\uB839 \uC904 \uC778\uC218 \uC77D\uAE30\uB294 \uC0AC\uC6A9\uC790\
  \uAC00 \uD504\uB85C\uADF8\uB7A8\uC744 \uC2DC\uC791\uD560 \uB54C \uC785\uB825\uD558\
  \uB294 \uD14D\uC2A4\uD2B8\uB97C \uC5BB\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uC0AC\uC6A9\uC790\uC758\
  \ \uC694\uAD6C\uC5D0 \uB9DE\uCD98 \uC720\uC5F0\uD55C \uBA85\uB839\uC5B4 \uC911\uC2EC\
  \uC758 \uD504\uB85C\uADF8\uB7A8\uC744 \uB9CC\uB4E4 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:56.018407-06:00'
model: gpt-4-1106-preview
summary: "\uBA85\uB839 \uC904 \uC778\uC218 \uC77D\uAE30\uB294 \uC0AC\uC6A9\uC790\uAC00\
  \ \uD504\uB85C\uADF8\uB7A8\uC744 \uC2DC\uC791\uD560 \uB54C \uC785\uB825\uD558\uB294\
  \ \uD14D\uC2A4\uD2B8\uB97C \uC5BB\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uC0AC\uC6A9\uC790\uC758 \uC694\
  \uAD6C\uC5D0 \uB9DE\uCD98 \uC720\uC5F0\uD55C \uBA85\uB839\uC5B4 \uC911\uC2EC\uC758\
  \ \uD504\uB85C\uADF8\uB7A8\uC744 \uB9CC\uB4E4 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

## 무엇 & 왜?
명령 줄 인수 읽기는 사용자가 프로그램을 시작할 때 입력하는 텍스트를 얻는 것입니다. 프로그래머들은 이를 통해 사용자의 요구에 맞춘 유연한 명령어 중심의 프로그램을 만들 수 있습니다.

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
