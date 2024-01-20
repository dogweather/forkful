---
title:                "디버그 출력을 인쇄하기"
html_title:           "Clojure: 디버그 출력을 인쇄하기"
simple_title:         "디버그 출력을 인쇄하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇이고 왜필요한가?

디버깅 출력은 프로그램의 처리 과정을 검사, 이해하기 위해 프로그래머가 출력하는 정보입니다. 이를 통해 코드가 정상 실행되는지, 문제가 있는지 확인할 수 있습니다.

## 어떻게 하나요:

Ruby에서는 표준 출력(stdout) 또는 표준 에러(stderr)로 디버그 정보를 출력할 수 있습니다. `puts`, `p`, `print`, `printf`, `pp` 등의 메소드도 확인해보세요.

```ruby
num = 5
p "Number is #{num}"
# => "Number is 5"

# or

require 'pp'
data = { name: 'Hong', age: 25, country: 'Korea' }
pp data
# => {:name=>"Hong", :age=>25, :country=>"Korea"}
```

## 깊어보기:

Ruby에서 디버그 출력을 위한 기능은 초기부터 있었습니다. 여기에는 제약이 많았지만 사용성과 효율성을 강조했습니다. 대안으로 알려진 것은 `logger` 라이브러리이며, 로그 레벨, 출력 형식, 출력 위치 등을 사용자가 정의할 수 있습니다.

```ruby
require 'logger'

logger = Logger.new(STDOUT)
logger.info "This is an information"
# I, [Date Time]  INFO -- : This is an information
```

또한 `binding.pry` 같은 디버거를 사용하면, 코드의 특정 지점에서 실행을 중지하고 환경을 검사할 수 있습니다.

## 참고 자료:

1. [DEBUGGING WITH RUBY](https://www.ruby-lang.org/en/documentation/debugging/)
2. [Ruby Logger](https://ruby-doc.org/stdlib/libdoc/logger/rdoc/Logger.html)
3. [pry](https://github.com/pry/pry)

이외에도 많은 디버그 라이브러리와 도구들이 있으니 참고하시기 바랍니다.