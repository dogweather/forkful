---
title:    "Ruby: 테스트 작성"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

루비 프로그래밍에서는 테스트를 작성할 이유가 많이 있습니다. 예를 들어, 애플리케이션을 변경하거나 새로운 기능을 추가할 때 이전 코드에 이상이 없는지 확인하거나 버그를 발견하는 데 도움이 됩니다. 또한 테스트를 작성함으로써 코드의 가독성이 개선되고 유지 보수가 쉬워집니다.

## 어떻게

루비에서는 테스트를 작성하고 실행하는 데 쉬운 방법이 있습니다. 다음 예제를 참고하여 실제로 테스트를 작성하는 방법을 배워보세요.

```Ruby
# 테스트를 위해 관례적으로 spec/ 폴더를 사용합니다.
# spec/calculator_spec.rb 파일을 생성하고 테스트 코드를 작성합니다.

require 'calculator' # 테스트할 코드 파일을 require 합니다.

describe Calculator do # Calculator 클래스를 describe 블록으로 감싸서 테스트를 정의합니다.

# #add 메소드를 테스트합니다.
  describe '#add' do
    it 'adds two numbers' do
      result = Calculator.add(2, 3) # Calculator의 클래스 메소드인 add를 호출하여 결과값을 변수에 저장합니다.
      expect(result).to eq(5) # 결과값이 5가 나오는지 검증합니다.
    end
  end

# #subtract 메소드를 테스트합니다.
  describe '#subtract' do
    it 'subtracts one number from another' do
      result = Calculator.subtract(5, 3)
      expect(result).to eq(2)
    end
  end
end
```

위 예제처럼 describe, it, expect 등의 키워드를 사용하여 테스트를 작성할 수 있습니다. 다음과 같이 콘솔에서 코드를 실행하면 테스트 결과를 확인할 수 있습니다.

```
$ rspec spec/calculator_spec.rb
```

테스트가 실패한 경우 적절한 오류 메시지와 함께 어떤 부분이 잘못되었는지 알려줍니다. 테스트가 모두 성공하면 코드를 안심하고 변경할 수 있습니다.

## 자세히 알아보기

테스트를 작성하는 방법은 다양하지만 일반적으로는 프로젝트의 각 부분을 테스트하는 여러 개의 파일을 작성합니다. 이렇게 하면 각각의 파일이 별도의 역할을 수행하며 코드를 이해하고 디버깅하기가 더 쉬워집니다.

또한 테스트를 작성할 때는 테스트 가능한 코드를 작성해야 합니다. 예를 들어, 전역 변수나 랜덤 값에 의존하는 코드는 테스트하기가 어렵습니다. 따라서 테스트 가능한 코드를 작성하면 유지 보수가 더 쉬워집니다.

마지막으로, 테스트하기 전에 코드를 작성하는 것과는 다른 마음가짐이 필요합니다. 코드를 작성할 때는 원하는 기능을 만들어내는 데 초점을 맞추지만, 테스트를 작성할 때는 코드가 제대로 작동하는지를 확인하는 데 초점을 맞추어야 합니다. 적절한 테스트를 작성하는 것이 코드의 신뢰성을 높이는 데 중요한 역할을 합니다.

## 관련 링크

- [RSpec 공식 홈페이지](https://rspec.info/)
- [Ruby 테스팅 가이드](https://rubypapa.com/2020/09/12/ruby-testing-overview/)
- [Ruby Step by Step: 테스