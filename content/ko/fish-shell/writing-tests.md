---
title:                "테스트 작성하기"
html_title:           "Fish Shell: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

### 무엇이고 왜?
테스트 작성이란 무엇인가요? 프로그래머들이 왜 이것을 하는 것일까요? 먼저, 테스트 작성은 코드의 동작을 확인하기 위해 작성되는 코드입니다. 즉, 작성한 코드가 원하는 대로 동작하는지 확인하는 것이죠. 프로그래머들은 이것을 하는 이유는 안정적이고 예측 가능한 코드를 작성하기 위해서입니다. 테스트 작성은 코드의 버그를 찾는 일종의 안전장치라고도 할 수 있습니다.

### 어떻게?
그럼 이제 실제로 어떻게 테스트를 작성하는지 살펴보겠습니다. 먼저, Fish Shell의 `assert` function을 사용하여 테스트를 작성할 수 있습니다. `assert`는 주어진 조건이 참인지 확인하는 함수로, 만약 조건이 거짓이면 에러를 발생시킵니다. 아래는 `assert`를 사용한 예시 코드입니다.

```Fish Shell
function add(a, b)
    return $a + $b
end

assert (add 2 3) -eq 5
```

위 코드에서 `assert` 함수는 `add` 함수에 2와 3을 넣은 결과가 5인지 확인합니다. 만약 결과가 5가 아니라면 에러가 발생할 것입니다. 이렇게 하면 코드의 동작을 확인하면서 버그를 미리 방지할 수 있습니다. 

### 깊이 들어가기
위에서 살펴본 `assert` 함수 이외에도 Fish Shell에서는 `test` 함수와 `and` 키워드를 사용하여 좀 더 복잡한 테스트를 작성할 수 있습니다. 또한 다른 Shell에서도 테스트를 작성하는 방법이 있지만 Fish Shell의 경우 가독성과 유연성 측면에서 좀 더 우수합니다.

### 참고 자료
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Shell scripting tutorial](https://www.shellscript.sh/)