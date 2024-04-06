---
date: 2024-01-20 17:53:33.165506-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Ruby\uC5D0\uC11C \uB514\
  \uBC84\uADF8 \uCD9C\uB825\uC740 `puts` \uB610\uB294 `p` \uBA54\uC11C\uB4DC\uB85C\
  \ \uAC04\uB2E8\uD788 \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC608\uC81C \uCF54\
  \uB4DC\uC640 \uCD9C\uB825 \uACB0\uACFC\uB97C \uBD05\uC2DC\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.559299-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Ruby\uC5D0\uC11C \uB514\uBC84\uADF8\
  \ \uCD9C\uB825\uC740 `puts` \uB610\uB294 `p` \uBA54\uC11C\uB4DC\uB85C \uAC04\uB2E8\
  \uD788 \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uCC0D\uC5B4\uBCF4\uAE30"
weight: 33
---

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
