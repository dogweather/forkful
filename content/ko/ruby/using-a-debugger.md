---
title:                "디버거 사용하기"
date:                  2024-01-26T04:09:58.299643-07:00
model:                 gpt-4-0125-preview
simple_title:         "디버거 사용하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/using-a-debugger.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Ruby에서 디버거를 사용하면 프로그래머들이 코드를 일시 정지하고, 변수를 검사하며, 코드를 한 줄씩 따라가며 검사할 수 있는 초능력을 가질 수 있습니다. 사람들은 버그를 잡고, 코드 흐름을 이해하며, 마법이 일어날 때—또는 일어나지 않을 때—자신이 작성한 주문(코드)이 정확히 무엇을 하는지 확인하기 위해 이를 사용합니다.

## 사용 방법:

Ruby에는 `byebug`이라는 내장 디버거가 있습니다. 먼저, Gemfile에 `byebug`을 포함시키고 `bundle install`을 실행합니다. 그런 다음, 프로그램이 잠시 멈추길 원하는 곳에 `byebug`을 배치하세요.

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

이 스크립트를 실행하면 `byebug`에서 실행이 멈추고 다음과 같은 명령어를 입력할 수 있는 대화형 세션으로 진입합니다:

```
step
next
continue
var local
```

샘플 출력은 이렇게 보일 것입니다:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## 심층 탐구:

`byebug` 이전에는, Ruby 사용자들이 `debugger`와 `pry`를 사용했습니다. 후자인 `pry`는 단순한 디버거 이상으로, `binding.pry` 중단점으로 디버깅에도 사용할 수 있는 강력한 REPL입니다.

Ruby의 `byebug`에 대한 대안으로는 `pry`와 `byebug` 기능을 결합한 `pry-byebug`와 더 이상 활발하게 유지 관리되지 않는 오래된 젬인 `ruby-debug`가 있습니다.

`byebug`을 호출하면, 디버거는 코드 실행을 중단시키고 런타임을 들여다볼 수 있게 해줍니다. 변수를 보고 변경하거나, 코드 내 다른 지점으로 점프하거나, 심지어 Ruby 코드를 한 줄씩 실행해볼 수도 있습니다. 마치 Ruby 코드에 대한 시간 여행 능력을 가진 것과 같습니다.

## 참고자료:

- Byebug GitHub 저장소: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Pry 문서: [https://github.com/pry/pry](https://github.com/pry/pry)
- Rails 앱 디버깅 가이드: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)