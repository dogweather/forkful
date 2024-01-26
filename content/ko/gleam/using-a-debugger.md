---
title:                "디버거 사용하기"
date:                  2024-01-26T03:49:16.117686-07:00
model:                 gpt-4-0125-preview
simple_title:         "디버거 사용하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디버거를 사용하는 것은 기본적으로 코드 속의 탐정 놀이를 하며 버그를 찾고 왜 문제가 발생하는지를 파악하는 과정입니다. 프로그래머가 이를 수행하는 이유는, 솔직히 말해서 버그는 불가피하며, 이를 효율적으로 해결함으로써 코드를 더 빠르고 더 신뢰성 있게 작동시킬 수 있기 때문입니다.

## 방법:
현재 Gleam은 도구 측면에서 Erlang 생태계에 기대고 있으므로, `rebar3`, `observer`, `debugger` 같은 도구를 사용해 디버깅을 주로 진행하게 됩니다. 디버깅에 직접 참여하는 방법은 다음과 같습니다:

```gleam
// rebar 설정에서 디버그 정보를 포함시키기 위해 이 라인들을 확인하세요:
{erl_opts, [debug_info]}.

// 앱이 로드된 Erlang 쉘을 실행하세요
rebar3 shell

// 쉘 내부에서 디버거를 시작할 수 있습니다
1> debugger:start().
```

간단하죠? `debugger` GUI가 나타나고, 여러분은 마음껏 중단점을 설정하고, 코드를 한 단계씩 진행하며, 변수를 관찰할 수 있습니다. 직접적으로 Gleam 코드를 보지는 못하지만, 이것이 컴파일되는 Erlang 코드를 볼 수 있으며, 이것은 여전히 상당히 유용합니다.

## 심층 분석
Gleam은 젊은 언어이므로, Erlang 생태계의 어깨 위에 서 있지만, 아직은 네이티브 Gleam 디버깅 도구가 주목받지 못하고 있습니다. 이는 우리가 Erlang의 검증된 도구들을 사용하고 있다는 의미이며, 이는 나쁜 일이 아닙니다. Erlang의 디버거는 90년대부터 있었으며, 신뢰성이 중요한 시스템에서 성가신 버그를 제거하는 데 수년 동안 갈고닦았습니다.

대안으로, 추적(Tracing)은 BEAM 세계(Erlang과 Elixir 코드를 실행하는 가상 머신)에서 강력한 방법입니다. `rebar3`을 사용하면 함수 호출을 추적하고 성능 문제를 깊이 파고들 수 있는 `recon` 같은 도구를 활용할 수 있습니다.

Gleam을 작성하는 것과 Erlang에서 디버깅하는 것 사이를 전환하는 것은 마치 생각을 즉석에서 번역하는 것처럼 느껴질 수 있습니다. 하지만 이점은 여러분이 앱의 런타임 형태에서 Erlang 세계를 엿보고, 앱의 구성 요소를 이해할 수 있다는 것입니다.

## 참고자료
디버깅 도구를 확장하려면 다음을 확인하세요:

- Erlang의 디버거 문서: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- Erlang을 위한 `recon` 라이브러리: [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
- BEAM에서의 추적에 대해: [https://adoptingerlang.org/docs/development/tracing/](https://adoptingerlang.org/docs/development/tracing/)