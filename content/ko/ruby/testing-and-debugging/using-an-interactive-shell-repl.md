---
date: 2024-01-26 04:17:36.349780-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Ruby\uC758 REPL\uC740 IRB(Interactive Ruby)\uB77C\
  \uACE0 \uBD88\uB9BD\uB2C8\uB2E4. \uD130\uBBF8\uB110\uC5D0\uC11C \uBC14\uB85C Ruby\uB97C\
  \ \uC2DC\uB3C4\uD574 \uBCF4\uC138\uC694."
lastmod: '2024-03-13T22:44:55.997001-06:00'
model: gpt-4-0125-preview
summary: "Ruby\uC758 REPL\uC740 IRB(Interactive Ruby)\uB77C\uACE0 \uBD88\uB9BD\uB2C8\
  \uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

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
