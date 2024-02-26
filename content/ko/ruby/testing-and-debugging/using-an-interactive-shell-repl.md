---
date: 2024-01-26 04:17:36.349780-07:00
description: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178, \uB610\uB294 REPL(Read-Eval-Print\
  \ Loop)\uC740 \uC2E4\uC2DC\uAC04\uC73C\uB85C \uCF54\uB4DC\uB97C \uD14C\uC2A4\uD2B8\
  \uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC774\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC804\uCCB4 \uC2A4\uD06C\uB9BD\
  \uD2B8\uB97C \uC791\uC131\uD558\uC9C0 \uC54A\uACE0\uB3C4 Ruby\uC758 \uBBF8\uBB18\
  \uD55C \uCC28\uC774\uC810\uC744 \uC2E4\uD5D8\uD558\uACE0, \uB514\uBC84\uADF8\uD558\
  \uBA70, \uBC30\uC6B8 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:52.977551-07:00'
model: gpt-4-0125-preview
summary: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178, \uB610\uB294 REPL(Read-Eval-Print\
  \ Loop)\uC740 \uC2E4\uC2DC\uAC04\uC73C\uB85C \uCF54\uB4DC\uB97C \uD14C\uC2A4\uD2B8\
  \uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC774\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC804\uCCB4 \uC2A4\uD06C\uB9BD\
  \uD2B8\uB97C \uC791\uC131\uD558\uC9C0 \uC54A\uACE0\uB3C4 Ruby\uC758 \uBBF8\uBB18\
  \uD55C \uCC28\uC774\uC810\uC744 \uC2E4\uD5D8\uD558\uACE0, \uB514\uBC84\uADF8\uD558\
  \uBA70, \uBC30\uC6B8 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
인터랙티브 셸, 또는 REPL(Read-Eval-Print Loop)은 실시간으로 코드를 테스트할 수 있게 해줍니다. 프로그래머들은 이를 사용하여 전체 스크립트를 작성하지 않고도 Ruby의 미묘한 차이점을 실험하고, 디버그하며, 배울 수 있습니다.

## 사용 방법:
Ruby의 REPL은 IRB(Interactive Ruby)라고 불립니다. 터미널에서 바로 Ruby를 시도해 보세요:

```Ruby
irb
2.7.0 :001 > puts "Hello, Ruby world!"
Hello, Ruby world!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## 깊이 탐구
Ruby 1.8에서 소개된 IRB는 Ruby 사용자에게 필수적인 도구입니다. Lisp와 Python의 인터랙티브 셸에서 영감을 받아, 실험과 즉각적인 피드백이 혼합된 형태입니다. 구문 강조 표시와 더 강력한 디버깅 환경과 같은 추가 기능을 제공하는 Pry와 같은 대안도 있습니다. IRB 자체는 단순하지만 'irbtools'와 같은 보석(gems)으로 기능을 확장할 수 있습니다. IRB가 read-eval-print 루프를 처리하는 방식은 입력된 각 줄을 읽고, Ruby 코드로 평가한 다음 결과를 출력하고, 이 과정을 종료까지 반복하는 것입니다.

## 참고 자료
- [Ruby의 IRB](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [irbtools 보석](https://github.com/janlelis/irbtools)
