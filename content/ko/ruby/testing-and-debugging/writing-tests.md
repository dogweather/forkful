---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:03.731849-07:00
description: "\uBC29\uBC95: Ruby\uC5D0\uB294 `Test::Unit`\uC774\uB77C\uB294 \uB0B4\
  \uC7A5 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uC788\uC5B4 \uB2E8\uC704 \uD14C\uC2A4\
  \uD2B8\uB97C \uC791\uC131\uD560 \uC218 \uC788\uC73C\uBA70, \uAC04\uB2E8\uD55C \uAD6C\
  \uC870 \uC548\uC5D0 \uD14C\uC2A4\uD305 \uAD00\uD589\uC744 \uCEA1\uC290\uD654\uD569\
  \uB2C8\uB2E4. \uADF8\uB7EC\uB098 Ruby \uCEE4\uBBA4\uB2C8\uD2F0\uB294 \uC885\uC885\
  \ RSpec\uACFC Minitest\uC640 \uAC19\uC740 \uD0C0\uC0AC \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB97C \uC120\uD638\uD569\uB2C8\uB2E4. \uC774\uB294 \uD45C\uD604\uB825\uACFC \uC720\
  \uC5F0\uC131\uC774\u2026"
lastmod: '2024-03-13T22:44:55.999741-06:00'
model: gpt-4-0125-preview
summary: "Ruby\uC5D0\uB294 `Test::Unit`\uC774\uB77C\uB294 \uB0B4\uC7A5 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uAC00 \uC788\uC5B4 \uB2E8\uC704 \uD14C\uC2A4\uD2B8\uB97C \uC791\
  \uC131\uD560 \uC218 \uC788\uC73C\uBA70, \uAC04\uB2E8\uD55C \uAD6C\uC870 \uC548\uC5D0\
  \ \uD14C\uC2A4\uD305 \uAD00\uD589\uC744 \uCEA1\uC290\uD654\uD569\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 방법:
Ruby에는 `Test::Unit`이라는 내장 라이브러리가 있어 단위 테스트를 작성할 수 있으며, 간단한 구조 안에 테스팅 관행을 캡슐화합니다. 그러나 Ruby 커뮤니티는 종종 RSpec과 Minitest와 같은 타사 라이브러리를 선호합니다. 이는 표현력과 유연성이 향상되기 때문입니다.

### `Test::Unit` 사용하기:
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    result = 2 + 2
    assert_equal 4, result
  end
end
```
터미널에서 테스트 파일을 실행하면, 테스트의 성공 또는 실패를 나타내는 출력이 나타납니다:
```
Loaded suite test_calculator
Started
.
Finished in 0.001288 seconds.
1 tests, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 omissions, 0 notifications
100% passed
```

### RSpec 사용하기:
RSpec은 Ruby를 위한 인기있는 BDD(Behavior-Driven Development) 프레임워크입니다. `gem install rspec`으로 gem을 설치한 다음, 프로젝트에서 `rspec --init`으로 초기화하세요.

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it '두 수를 올바르게 더한다' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
`rspec` 명령어로 테스트를 실행하세요. 예시 출력:
```
.

Finished in 0.002 seconds (files took 0.1 seconds to load)
1 example, 0 failures
```

### Minitest 사용하기:
Minitest는 TDD, BDD, 목킹, 벤치마킹을 지원하는 테스팅 시설의 완전한 스위트를 제공합니다. `gem install minitest`로 설치하고 다음과 같이 사용하세요:

```ruby
# test_calculator.rb
require 'minitest/autorun'
require_relative '../calculator'

class CalculatorTest < Minitest::Test
  def test_addition
    assert_equal 4, Calculator.add(2, 2)
  end
end
```

테스트 파일을 직접 실행하거나 minitest를 위해 설정된 `rake` 작업을 통해 실행하세요. 샘플 출력:
```
Run options: --seed 33407

# Running:

.

Finished in 0.001027s, 974.5922 runs/s, 974.5922 assertions/s.
1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

이러한 라이브러리를 사용하여 Ruby 프로젝트에서 테스트를 구현함으로써, 당신은 모범 사례를 준수하게 되며, 더 신뢰성 있고 유지보수가 용이한 코드 베이스를 이끌게 됩니다.
