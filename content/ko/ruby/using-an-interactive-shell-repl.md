---
title:                "인터랙티브 셸 (REPL) 사용하기"
aliases:
- ko/ruby/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:17:36.349780-07:00
model:                 gpt-4-0125-preview
simple_title:         "인터랙티브 셸 (REPL) 사용하기"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/using-an-interactive-shell-repl.md"
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
