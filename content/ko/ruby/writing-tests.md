---
title:    "Ruby: 테스트 작성"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## 왜

코드를 작성하고 테스트를 작성하는 이유는 소프트웨어의 더 나은 품질과 안정성을 보장하기 위함입니다.

## 어떻게

이제 우리는 어떻게 Ruby에서 첫 번째 테스트를 작성하는지 배워볼 것입니다. 아래 예제를 따라해 보세요.

```Ruby
# 새로운 테스트 파일 생성
test.rb

# 테스트 파일에 MyCalculator 클래스 추가
class MyCalculator
  # 두 수를 더하는 메소드 추가
  def add(x, y)
    x + y
  end
end

# 테스트 파일에서 MiniTest 사용
require 'minitest/autorun'

# 테스트 시작
class MyCalculatorTest < MiniTest::Test
  # 더하기 테스트
  def test_add
    # MyCalculator 클래스의 add 메소드를 테스트
    assert_equal(4, MyCalculator.new.add(2, 2))
  end
end

```

위 예제를 실행하면 아래와 같은 결과가 출력될 것입니다.

```Ruby
Run options: --seed 11813

# Running:

.

Finished in 0.001470s, 680.2722 runs/s, 680.2722 assertions/s.

1 runs, 1 assertions, 0 failures, 0 errors, 0 skips

```

위에서 볼 수 있듯이, 우리의 더하기 메소드가 올바르게 작동하는 것을 확인할 수 있습니다.

## 딥 다이브

테스트 작성에 대한 더 깊은 정보를 얻고 싶다면 아래 링크를 참고해 보세요.

- [Ruby 공식 문서](https://ruby-doc.org/stdlib-2.6.3/libdoc/minitest/rdoc/MiniTest/Test.html)
- [Ruby Kwalify 테스트 작성 방법](https://www.rubydoc.info/github/jystewart/kwalify/Test)
- [RSpec를 이용한 테스트 작성 방법](https://rspec.info/documentation/)

## 이 외에도

- [MiniTest RubyGems 페이지](https://rubygems.org/gems/minitest)
- [MiniTest 튜토리얼](https://minitest.rubystyle.guide/)