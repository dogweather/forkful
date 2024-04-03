---
date: 2024-01-20 17:53:33.165506-07:00
description: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC740 \uCF54\uB4DC\uC758 \uC791\uB3D9\
  \uC744 \uD655\uC778\uD558\uAE30 \uC704\uD574 \uC911\uAC04 \uACB0\uACFC\uB97C \uCF58\
  \uC194\uC5D0 \uCC0D\uC5B4\uC8FC\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uBC84\uADF8\uB97C \uCC3E\uACE0, \uD504\uB85C\uADF8\uB7A8\
  \uC758 \uD750\uB984\uC744 \uC774\uD574\uD558\uAE30 \uC704\uD574 \uC774 \uBC29\uBC95\
  \uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.998275-06:00'
model: gpt-4-1106-preview
summary: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC740 \uCF54\uB4DC\uC758 \uC791\uB3D9\uC744\
  \ \uD655\uC778\uD558\uAE30 \uC704\uD574 \uC911\uAC04 \uACB0\uACFC\uB97C \uCF58\uC194\
  \uC5D0 \uCC0D\uC5B4\uC8FC\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uCC0D\uC5B4\uBCF4\uAE30"
weight: 33
---

## What & Why? (무엇인가요? 왜 사용하나요?)
디버그 출력은 코드의 작동을 확인하기 위해 중간 결과를 콘솔에 찍어주는 것입니다. 프로그래머들은 버그를 찾고, 프로그램의 흐름을 이해하기 위해 이 방법을 사용합니다.

## How to: (어떻게 하나요?)
Ruby에서 디버그 출력은 `puts` 또는 `p` 메서드로 간단히 할 수 있습니다. 예제 코드와 출력 결과를 봅시다.

```Ruby
# 변수 값 확인하기
number = 42
puts "The number is #{number}"
# => The number is 42

# 배열 내용 출력하기
fruits = ["apple", "banana", "cherry"]
p fruits
# => ["apple", "banana", "cherry"]

# 조건문 디버깅
age = 25
if age < 20
  puts "You're a teenager!"
else
  puts "You're an adult!"
end
# => You're an adult!
```

## Deep Dive (심층 분석)
디버그 출력은 프로그래밍 초기부터 사용됐습니다. `puts`는 단순히 문자열을 출력하지만, `p`는 인자의 `.inspect` 메서드를 호출하여 더 많은 정보를 제공합니다.

대안으로는 `print` 메서드가 있지만, 이는 개행을 포함하지 않습니다. `Logger` 클래스를 사용하여 파일에 로그를 남길 수도 있습니다. 이 방법은 프로덕션 코드에서 추적할 때 유용합니다.

```Ruby
require 'logger'

logger = Logger.new('debug.log')
logger.debug("Program started")
logger.info("Processing data...")
# ... 코드 실행 ...
logger.warn("Done with some warnings.")
logger.error("Something went wrong!")
```

`Logger` 클래스는 `debug`, `info`, `warn`, `error`, `fatal`의 다양한 수준으로 로그를 남깁니다.

## See Also (참조하기)
- Ruby 공식 문서: [Kernel#puts](https://ruby-doc.org/core-3.1.2/Kernel.html#method-i-puts)
- Ruby 공식 문서: [Kernel#p](https://ruby-doc.org/core-3.1.2/Kernel.html#method-i-p)
- Ruby 공식 문서: [Logger](https://ruby-doc.org/stdlib-3.1.2/libdoc/logger/rdoc/Logger.html)
