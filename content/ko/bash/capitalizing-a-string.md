---
title:    "Bash: 문자열 대문자 변환"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 왜 Capitalizing String In Bash를 쓸까요?

Bash 프로그래밍은 많은 사람들에게 친숙하지 않지만, 요즘 우리의 일상에서 자주 사용되는 언어 중 하나입니다. 하지만 어떤 경우에는 이 언어를 사용하면서도 조금 번거로운 일이 발생할 수 있습니다. 가령, 일반적인 텍스트 작업 중 문자열을 대문자로 변환하고 싶을 때가 있을 겁니다. 이때 Bash의 강력한 기능인 "Capitalizing String"을 사용하면 매우 편리합니다.

# 사용 방법

간단한 예제를 통해 어떻게 Bash에서 "Capitalizing String"을 사용하는지 살펴보겠습니다.

```Bash
# 문자열을 변수로 선언합니다.
str="hello world"

# 문자열을 대문자로 변환합니다.
str=${str^^}

# 변환된 문자열을 출력합니다.
echo $str
```

위 코드를 실행하면 "HELLO WORLD"라는 출력 결과를 볼 수 있습니다. 여기서 중요한 점은 문자열을 변환하는 법을 기억해두는 것입니다. 변수를 사용해야 하고, 변수를 한 번 더 사용하여 변환된 값으로 할당해야 합니다.

# 더 깊이 알아보기

"Capitalizing String"은 Bash shell에서 문자열을 다루는 여러 가지 함수 중 하나입니다. 이 함수를 사용하면 문자열을 소문자나 대문자로 변환할 수 있습니다. "^^"를 사용하면 대문자로, "^^"를 사용하면 소문자로 변환됩니다.

# See Also

- https://wiki.bash-hackers.org/commands/builtin/uppercase
- https://linuxhint.com/capitalize_string_bash/
- https://linuxize.com/post/bash-string/