---
title:                "테스트 작성하기"
html_title:           "Ruby: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

테스트를 작성하는 일의 이유는 코드를 더 나은 품질로 유지하고 버그를 방지하기 위해서입니다.

## 어떻게 하나요

우선 `RSpec`를 설치해야 합니다. 이를 위해서는 터미널에서 다음 명령어를 실행하면 됩니다:

``` Ruby
gem install rspec
```

이제 `spec` 폴더를 만들어줍니다. 거기에 테스트 파일을 생성합니다. 예를 들어, `calculator_spec.rb`라는 이름의 파일을 만들고, 다음과 같은 코드를 작성합니다:

``` Ruby
require './calculator' # calculator.rb 파일이 같은 폴더에 있을 때

RSpec.describe Calculator do
    describe "#add" do
        it "adds two numbers" do
            expect(Calculator.add(1, 5)).to eq(6)
        end
    end
end
```

위의 코드에서는 `Calculator` 클래스의 `add` 메소드를 테스트하는 예제입니다.

그리고 `calculator.rb` 파일을 만들어주고, 다음과 같은 코드를 작성합니다:

``` Ruby
class Calculator
    def self.add(a, b)
        a + b
    end
end
```

이제 터미널에서 `rspec spec` 명령어를 실행하면, 테스트가 실행되고 결과가 출력됩니다.

```
Finished in 0.00105 seconds (files took 0.07245 seconds to load)
1 example, 0 failures
```

따라서 위의 예제에서는 `Calculator` 클래스의 `add` 메소드가 제대로 작동하는지를 확인할 수 있습니다.

## Deep Dive

테스트를 작성하는 더 깊은 이유는 코드를 변화시켰을 때 예상치 못한 버그를 방지하기 위해서입니다. 테스트가 있으면 코드를 수정하는 과정에서도 코드가 예상대로 작동하는지를 확인할 수 있습니다. 

또한 테스트를 작성하면 코드의 구조를 더 명확하고 유지보수하기 쉽게 만들어줍니다. 테스트를 작성하면서 코드의 각 부분이 어떤 역할을 하고 어떻게 상호작용하는지를 파악할 수 있습니다. 따라서 코드의 가독성도 높아지고, 개발 과정에서 발생할 수 있는 실수도 예방할 수 있습니다.

## See Also

- [RSpec Official Website](https://rspec.info/)
- [Learn Ruby in Y Minutes](https://learnxinyminutes.com/docs/ruby/)