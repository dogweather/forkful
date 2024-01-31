---
title:                "에러 처리하기"
date:                  2024-01-26T00:56:51.775580-07:00
model:                 gpt-4-1106-preview
simple_title:         "에러 처리하기"

category:             "Ruby"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/handling-errors.md"
---

{{< edit_this_page >}}

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
