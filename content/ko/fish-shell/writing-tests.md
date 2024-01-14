---
title:                "Fish Shell: 프로그래밍 테스트 작성하기"
simple_title:         "프로그래밍 테스트 작성하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## 왜
테스트를 작성하는 것에 참여하는 이유는 무엇일까요? 테스트를 작성하면서 코드를 더욱 발전시키고 품질을 향상시킬 수 있습니다.

## 이렇게 하세요
테스트 코드를 작성하는 것은 매우 간단합니다. 다음과 같은 예제 코드와 출력을 확인해보세요.

```Fish Shell
# 테스트를 위한 함수 정의
function add  # 두 숫자를 더하는 함수
  echo $argv[1] + $argv[2]  # 첫 번째와 두 번째 파라미터를 더한 값을 출력
end

# 함수 호출과 예상 결과 비교
# 예상한 결과: 5
# 실제 결과: 6
begin
  add 2 3
end
```

출력:
`5`

위와 같이 코드를 작성하고 실행해보면 테스트 결과가 예상한 것과 다르면 에러가 발생하는 것을 확인할 수 있습니다. 이렇게 테스트를 작성하면 코드의 이상을 더 빨리 발견하고 수정할 수 있습니다.

## 딥 다이브
테스트를 작성할 때 주의해야 할 몇 가지 규칙이 있습니다. 먼저, 매번 같은 결과를 얻기 위해서는 테스트를 실행할 때마다 같은 환경에서 코드가 실행되어야 합니다. 그리고 각 테스트는 독립적이어야 하며 다른 테스트와 상호작용하지 않도록 작성되어야 합니다. 또한 테스트가 실패할 경우 원인을 빠르게 파악할 수 있도록 테스트 메시지를 자세히 작성하는 것이 중요합니다.

## See Also
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHub 저장소](https://github.com/fish-shell/fish-shell)
- [Fish Shell 테스트 코드 작성 예제](https://github.com/fisherman/scripting-fish)