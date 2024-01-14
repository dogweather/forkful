---
title:    "Fish Shell: 테스트 작성하기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## 왜?

코딩 테스트를 쓰는 이유는 무엇일까요? 이 질문에 대한 답은 간단합니다. 코드를 작성할 때 기능적인 부분을 확실하게 테스트하는 것은 중요합니다. 코드를 작성하는 시간에 비해 테스트를 작성하는 시간은 매우 짧을 수 있지만, 코드를 테스트하는 것은 오랜 시간동안 코드를 관리하는 데 큰 도움이 될 수 있습니다.

## 실제로 어떻게 쓸까요?

먼저, Fish Shell을 설치해야 합니다. 그 다음 다음 코드를 사용하여 간단한 테스트를 작성해보세요.

```Fish Shell
function add(num1, num2)
    return $num1 + $num2
end

echo (add 3 5)
```

위의 코드는 `8`을 출력할 것입니다. 이와 같이 Fish Shell을 사용하면 간단하고 빠르게 테스트를 작성할 수 있습니다. 또한 Fish Shell은 다른 언어와 호환되므로 기존에 작성한 테스트를 쉽게 이식할 수 있습니다.

## 심층 탐구

모든 개발자는 코드를 작성할 때 완벽한 코드를 작성하고 싶어합니다. 그래서 테스트를 작성하는 것이 중요합니다. 많은 개발자들이 테스트를 작성하는 것을 게을레하게 여기며 무시하고 있지만, 테스트를 작성함으로써 코드의 버그를 미리 예방할 수 있고, 코드를 수정할 때 안심하고 수정할 수 있습니다. 또한 테스트를 작성하면 코드의 활용성을 높일 수 있습니다.

## 또 다른 자료

- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [Fish Shell 테스트 관련 문서](https://fishshell.com/docs/current/index.html#tests)
- [Fish Shell 테스트 예제](https://github.com/fish-shell/fish-shell/tree/master/test)