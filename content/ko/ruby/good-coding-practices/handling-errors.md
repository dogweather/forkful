---
date: 2024-01-26 00:56:51.775580-07:00
description: "\uC624\uB958 \uCC98\uB9AC\uB780 \uCF54\uB4DC\uC5D0\uC11C \uC608\uAE30\
  \uCE58 \uC54A\uC740 \uC77C\uC744 \uC608\uC0C1\uD558\uACE0 \u2014 \uD06C\uB798\uC2DC\
  \ \uC5C6\uC774 \uC6B0\uC544\uD558\uAC8C \uC2E4\uC218\uC640 \uBB38\uC81C\uB97C \uAD00\
  \uB9AC\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294\
  \ \uC77C\uC774 \uC798\uBABB \uAC14\uC744 \uB54C \uD750\uB984\uC744 \uC81C\uC5B4\uD558\
  \uACE0 \uC0AC\uC6A9\uC790 \uACBD\uD5D8\uC744 \uBD80\uB4DC\uB7FD\uAC8C \uC720\uC9C0\
  \uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:56.006431-06:00'
model: gpt-4-1106-preview
summary: "\uC624\uB958 \uCC98\uB9AC\uB780 \uCF54\uB4DC\uC5D0\uC11C \uC608\uAE30\uCE58\
  \ \uC54A\uC740 \uC77C\uC744 \uC608\uC0C1\uD558\uACE0 \u2014 \uD06C\uB798\uC2DC \uC5C6\
  \uC774 \uC6B0\uC544\uD558\uAC8C \uC2E4\uC218\uC640 \uBB38\uC81C\uB97C \uAD00\uB9AC\
  \uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC77C\
  \uC774 \uC798\uBABB \uAC14\uC744 \uB54C \uD750\uB984\uC744 \uC81C\uC5B4\uD558\uACE0\
  \ \uC0AC\uC6A9\uC790 \uACBD\uD5D8\uC744 \uBD80\uB4DC\uB7FD\uAC8C \uC720\uC9C0\uD558\
  \uAE30 \uC704\uD574 \uC774\uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
weight: 16
---

## 무엇 & 왜?

오류 처리란 코드에서 예기치 않은 일을 예상하고 — 크래시 없이 우아하게 실수와 문제를 관리하는 것입니다. 프로그래머는 일이 잘못 갔을 때 흐름을 제어하고 사용자 경험을 부드럽게 유지하기 위해 이를 수행합니다.

## 방법:

루비는 오류를 처리하기 위해 `begin`, `rescue`, `ensure`, `end`를 사용합니다. 위험한 코드를 `begin`과 `end`로 감쌉니다. 오류가 발생하면 `rescue`가 작동합니다.

```Ruby
begin
  # 위험한 코드가 여기에 옵니다.
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "이런! 그건 할 수 없습니다: #{e.message}"
ensure
  puts "오류가 발생하든 안 하든 항상 실행됩니다."
end
```

출력 예시:
```
이런! 그건 할 수 없습니다: 0으로 나눔
오류가 발생하든 안 하든 항상 실행됩니다.
```

## 깊이 있게 탐구

역사적으로, 프로그래밍 언어의 오류 처리는 상당히 발전해 왔으며, 초기 언어들은 종종 원시적이거나 존재하지 않는 메커니즘을 가지고 있었습니다. 루비의 예외 처리는 파이썬이나 스몰토크와 같은 언어들로부터 영감을 받았습니다.

루비에서 `begin-rescue`의 대안으로는 메소드 정의에서 `rescue`를 사용하거나, 비표준 흐름 제어를 위해 `throw`와 `catch`를 사용하는 것이 있지만, 일반적인 오류 처리에는 사용되지 않습니다.

흥미로운 세부 사항 중 하나: 루비의 예외는 객체입니다 (`Exception` 클래스 및 그 후손의 인스턴스), 그래서 사용자 정의 오류 클래스를 정의하고 단순히 오류를 로그하는 것 이상을 할 수 있습니다 — 프로그램 전반에 걸쳐 더 견고한 오류 처리를 위한 풍부한 상태를 전달할 수 있습니다.

## 참고자료

- 예외와 오류 처리에 관한 루비 문서: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- 루비 오류 처리 모범 사례에 관한 자세한 가이드: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
