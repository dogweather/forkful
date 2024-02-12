---
title:                "디버그 출력을 찍어보기"
aliases:
- ko/ruby/printing-debug-output.md
date:                  2024-01-20T17:53:33.165506-07:00
model:                 gpt-4-1106-preview
simple_title:         "디버그 출력을 찍어보기"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

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
