---
date: 2024-01-26 04:09:58.299643-07:00
description: "Ruby\uC5D0\uB294 `byebug`\uC774\uB77C\uB294 \uB0B4\uC7A5 \uB514\uBC84\
  \uAC70\uAC00 \uC788\uC2B5\uB2C8\uB2E4. \uBA3C\uC800, Gemfile\uC5D0 `byebug`\uC744\
  \ \uD3EC\uD568\uC2DC\uD0A4\uACE0 `bundle install`\uC744 \uC2E4\uD589\uD569\uB2C8\
  \uB2E4. \uADF8\uB7F0 \uB2E4\uC74C, \uD504\uB85C\uADF8\uB7A8\uC774 \uC7A0\uC2DC \uBA48\
  \uCD94\uAE38 \uC6D0\uD558\uB294 \uACF3\uC5D0 `byebug`\uC744 \uBC30\uCE58\uD558\uC138\
  \uC694. ```Ruby require\u2026"
lastmod: '2024-03-13T22:44:56.001380-06:00'
model: gpt-4-0125-preview
summary: "Ruby\uC5D0\uB294 `byebug`\uC774\uB77C\uB294 \uB0B4\uC7A5 \uB514\uBC84\uAC70\
  \uAC00 \uC788\uC2B5\uB2C8\uB2E4. \uBA3C\uC800, Gemfile\uC5D0 `byebug`\uC744 \uD3EC\
  \uD568\uC2DC\uD0A4\uACE0 `bundle install`\uC744 \uC2E4\uD589\uD569\uB2C8\uB2E4.\
  \ \uADF8\uB7F0 \uB2E4\uC74C, \uD504\uB85C\uADF8\uB7A8\uC774 \uC7A0\uC2DC \uBA48\uCD94\
  \uAE38 \uC6D0\uD558\uB294 \uACF3\uC5D0 `byebug`\uC744 \uBC30\uCE58\uD558\uC138\uC694\
  . ```Ruby require\u2026"
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
weight: 35
---

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
