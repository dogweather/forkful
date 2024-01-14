---
title:                "Fish Shell: 테스트 작성하기"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

코딩을 할 때, 많은 비용과 노력이 들어가는 일이 많습니다. 그리고, 버그를 찾기 위해 많은 시간과 노력을 투자해야 할 수도 있습니다. 그래서 테스트를 작성하는 것은 매우 중요한 일이 됩니다. 테스트를 작성하는 것은 우리의 코드가 오류 없이 잘 작동하는지 확인하기 위해 필요한 단계입니다. 이는 시간과 비용을 절약해주고, 더 나은 코드를 작성하는 데에도 도움이 됩니다.

## 하는 방법

### Fish Shell을 이용한 테스트 작성 예시

```
fish_prompt="Hello World"

describe "fish_prompt"
    it "should print 'Hello World'"
        test "$fish_prompt" = "Hello World"
    end
end
```

위의 예시는 `fish_prompt`이 정말로 "Hello World"인지 확인하는 테스트 코드입니다. 일단 이 코드를 작성하고 저장한 다음, 다음과 같이 터미널에 입력하면 테스트 결과를 볼 수 있습니다.

```
fish 테스트.fish
```

아마도 결과는 다음과 비슷할 것입니다.

```
fish_prompt should print 'Hello World'
```

위의 결과는 우리가 직접 테스트 코드를 작성하고 테스트를 실행해봄으로써 우리의 코드가 올바르게 작동하는지 확인할 수 있게 해줍니다.

## 깊이 있는 탐구

위의 예시에서는 단순히 `fish_prompt`가 "Hello World"인지를 확인했지만, 우리는 다양한 테스트를 작성할 수 있습니다. 예를 들어, `fish_prompt`가 특정 패턴을 따르는지, 특정 변수에 올바른 값을 가지고 있는지 등을 확인할 수 있습니다. 이렇게 다양한 테스트를 작성하고 실행해봄으로써 우리의 코드를 더욱 견고하고 오류 없이 작동하도록 만들 수 있습니다.

## See Also

- [Fish Shell 메뉴얼](https://fishshell.com/docs/current)
- [Fish Shell 테스트 프레임워크 예시](https://github.com/fisheryou/erikhuda-thor/wiki/Testing-Fish-Scripts)

테스트를 작성하는 것은 어려운 일이 아닙니다. 하지만, 우리의 코드가 정말로 올바르게 작동하는지 확인하기 위해 필요한 중요한 단계입니다. Fish Shell의 간단한 테스트 프레임워크를 이용하면 우리의 코드가 얼마나 잘 작동하는지 확인할 수 있습니다. 더 나은 코드를 작성하고, 더 나은 프로그래머가 되기 위해 테스트를 작성하는 것을 잊지 마세요!