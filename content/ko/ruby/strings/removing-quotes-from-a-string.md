---
date: 2024-01-26 03:41:40.412628-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\
  \uD55C\uB2E4\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8 \uAC12\uC744 \uAC10\uC2F8\uACE0\
  \ \uC788\uB294 \uADF8 \uB354\uBE14 \uB530\uC634\uD45C\uB098 \uC2F1\uAE00 \uB530\uC634\
  \uD45C\uB97C \uBC97\uACA8\uB0B4\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uC0AC\uC6A9\uC790 \uC785\
  \uB825\uC744 \uC815\uB9AC\uD558\uAC70\uB098, \uB370\uC774\uD130 \uCC98\uB9AC\uC758\
  \ \uC77C\uAD00\uC131\uC744 \uBCF4\uC7A5\uD558\uAC70\uB098, \uCD94\uAC00 \uBB38\uC790\
  \uC5D0 \uD63C\uB780\uC744 \uACAA\uC744 \uC218 \uC788\uB294 \uC2DC\uC2A4\uD15C\uC744\
  \ \uC704\uD574 \uB370\uC774\uD130\uB97C \uC900\uBE44\uD558\uAE30 \uC704\uD574 \uC774\
  \u2026"
lastmod: '2024-03-13T22:44:55.974633-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8 \uAC12\uC744 \uAC10\uC2F8\uACE0 \uC788\
  \uB294 \uADF8 \uB354\uBE14 \uB530\uC634\uD45C\uB098 \uC2F1\uAE00 \uB530\uC634\uD45C\
  \uB97C \uBC97\uACA8\uB0B4\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
weight: 9
---

## 어떻게:
Ruby는 이런 성가신 따옴표를 잘라내는데 몇 가지 깔끔한 트릭을 가지고 있습니다. `gsub`나 `delete` 메소드를 사용해 작업을 완료할 수 있습니다. 여기 좀 물어보세요:

```ruby
# gsub을 사용해 더블 따옴표와 싱글 따옴표 제거
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# 출력: Say hello to my little friend!

# 한 종류의 따옴표만 다룬다는 것을 알고 있다면
single_quoted_string = "'Stay a while and listen!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# 출력: Stay a while and listen!
```

## 심층 탐구
따옴표의 역사는 문자열 구분자로서 종종 사용되었던 프로그래밍의 초기 날로 거슬러 올라갑니다. 지금처럼, 그때 당시에도 필요하지 않거나 데이터 저장 및 조작을 방해할 수 있는 이런 따옴표 문자를 제거해야 할 수도 있습니다.

`gsub`과 `delete`에 대해 이야기했지만 `tr`이나 `tr_s`와 같은 다른 메소드도 있습니다. 이는 조금 더 많은 제어를 제공하거나 다른 사용 사례를 처리할 수 있습니다:

```ruby
# tr도 따옴표를 제거할 수 있음
double_quoted_string = "\"Do or do not, there is no try.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# 출력: Do or do not, there is no try.
```

이들 각각의 메소드가 사용 사례가 있다는 것을 기억하세요. `gsub`은 복잡한 패턴이나 다중 치환을 다룰 때 더 강력합니다. `delete`와 `tr`은 단순하고 직접적인 문자 제거에 아름답게 작동합니다.

## 참고 문헌
더 많은 정보와 이 메소드들이 더 큰 코드베이스 내에서 작동하는 것을 보기 위해 참고하세요:
- [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete), 그리고 [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr)에 대한 루비 문서.
- Ruby Monstas는 따옴표를 다루는 것을 포함한 훌륭한 [String 연습 세트](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html)를 가지고 있습니다.
- [문자열 조작](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string)에 대한 스택 오버플로우 토론은 동료 Rubyist들로부터 실제 문제와 해결책을 제공합니다.
