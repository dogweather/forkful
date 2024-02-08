---
title:                "디버거 사용하기"
aliases:
- ko/fish-shell/using-a-debugger.md
date:                  2024-01-26T03:50:15.130206-07:00
model:                 gpt-4-0125-preview
simple_title:         "디버거 사용하기"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/using-a-debugger.md"
---

{{< edit_this_page >}}

## 무엇을, 왜 사용하나요?
디버거를 사용하는 것은 모두 버그를 제거하는 것에 관한 것입니다—코드 내의 불쾌하고 시간을 빨아들이는 오류들. 프로그래머들이 디버깅하는 이유는 효율적으로 문제를 찾아 해결하기 위해서, 코드 흐름을 이해하고, 자신의 코드가 실제로 무엇을 하는지 더 명확하게 파악하기 위해서입니다.

## 사용 방법:
Fish는 다른 쉘들처럼 내장된 디버거가 없지만, 컴파일된 프로그램을 디버깅하기 위해 `gdb` 같은 외부 도구를 사용하거나, 다양한 레벨에서 디버그 출력과 함께 fish를 실행하기 위해 `fish -d`를 사용할 수 있습니다. `fish -d`를 사용해 보겠습니다:

```fish
# 디버그 레벨 2로 fish 쉘 실행
fish -d2

# fish 쉘에서 잠재적 버그를 가진 간단한 함수를 테스트해봅시다
function test_func
    set val 42
    echo "값은 $val입니다"
    if test $val -eq 42
        echo "모든 것이 잘 되었습니다."
    else
        echo "뭔가 수상합니다."
    end
end

# 함수를 호출하고 디버그 출력을 관찰합니다
test_func
```

함수가 실행되기 전후로 추가 디버그 출력을 볼 수 있으며, 이를 통해 문제를 정확히 파악할 수 있습니다.

## 심층 분석
역사적으로, Unix와 유사한 환경에서의 디버깅은 `gdb` (C/C++용)나 `pdb` (Python용) 같은 특수 도구의 영역이었습니다. Fish에서는, 일반적으로 외부 유틸리티나 함수에 대한 자세한 출력을 제공하는 `functions -v`, 변수 변경을 추적하는 `set -x` 같은 내장 기능에 의존합니다.

일부 사람들은 스크립트 디버깅 기능인 `set -x` 때문에 Bash와 같은 다른 쉘을 선택하기도 합니다. 그러나, Fish는 사용자 친화성과 상호작용성에 중점을 두어 많은 경우 하드코어 디버깅의 필요성을 줄일 수 있습니다.

구현에 있어, 스크립트 디버깅은 종종 자세한 출력으로 실행하고 변수가 예기치 않은 방식으로 설정되거나 해제되거나 변경되는 위치를 추적하는 것을 포함합니다. Fish의 색코드 출력과 사용자 친화적 접근 방식으로, 디버깅의 까다로운 부분을 종종 피할 수 있지만 – 당신이 막히었을 때, 기억하세요, 장황함과 명확함이 최고의 도구입니다.

## 참고 자료
코드에 빠졌을 때 도움이 될 신뢰할 수 있는 생명줄입니다:

- Fish 문서 디버깅: https://fishshell.com/docs/current/index.html#debugging
- GDB(GNU 디버거) 공식 가이드: https://www.gnu.org/software/gdb/documentation/
- Stack Overflow Fish 태그 - 실제 디버깅 사례: https://stackoverflow.com/questions/tagged/fish
- 고급 Bash 스크립팅 가이드 - 디버깅 접근 방법 비교: https://tldp.org/LDP/abs/html/debugging.html
