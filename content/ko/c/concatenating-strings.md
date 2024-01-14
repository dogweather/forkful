---
title:    "C: 문자열 연결하기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열 연결(concatenating)이 필요한 이유는 여러 문자열을 하나로 합쳐서 사용해야 할 때입니다. 예를 들어, 이름과 성을 따로 입력받았지만 나중에는 이름과 성을 합쳐서 출력해야 할 때가 있습니다. 이럴 때 문자열 연결을 사용하면 편리하고 간편합니다.

## 방법

문자열 연결에는 `strcat()` 함수를 사용합니다. 이 함수는 `string.h` 라이브러리에 포함되어 있으며 다음과 같은 형태를 가집니다.

```C
strcat(결합할 문자열, 추가할 문자열);
```

결합할 문자열에는 추가할 문자열이 뒤에 이어붙여지게 됩니다. 아래는 실제 사용 예시입니다.

```C
#include <stdio.h>
#include <string.h>

int main() {
	// 이름과 성을 입력받는 예제
	char name[10], surname[10];
	printf("이름을 입력하세요: ");
	scanf("%s", name);
	printf("성을 입력하세요: ");
	scanf("%s", surname);

	// 두 문자열을 합쳐줍니다.
	strcat(name, surname);

	// 합쳐진 문자열을 출력합니다.
	printf("이름과 성이 합쳐진 결과: %s", name);

	return 0;
}
```

위 코드를 실행하면 아래와 같은 결과를 얻을 수 있습니다.

```
이름을 입력하세요: 홍
성을 입력하세요: 길동
이름과 성이 합쳐진 결과: 홍길동
```

## 딥 다이브

문자열 연결은 기본적으로 결합할 문자열에 추가할 문자열을 덧붙이도록 설계되어 있습니다. 하지만 `strncat()` 함수를 사용하면 추가할 문자열의 일부분만 결합할 수도 있습니다. `strncat()` 함수의 형태는 다음과 같습니다.

```C
strncat(결합할 문자열, 추가할 문자열, 추가할 문자열의 일부분 길이);
```

추가할 문자열의 일부분 길이는 작성된 문자열의 길이를 넘지 않아야 합니다. 예를 들어, "Hello, World!"라는 문자열에서 "Hello"만 결합할 수 있습니다.

```C
#include <stdio.h>
#include <string.h>

int main() {
	// "Hello, World!" 문자열
	char str[50] = "Hello, ";
	char hello[50] = "Hello";
	char world[50] = "World!";

	// "Hello"만 결합
	strncat(str, hello, 5);

	// 결과 출력
	printf("결과: %s", str);

	return 0;
}
```

위 코드를 실행하면 "Hello, Hello"가 출력됩니다.

## 연관 정보

- `strcat()` 함수 문서: https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm
- `strncat()` 함수 문서: https://www.tutorialspoint.com/c_standard_library/c_function_strncat.htm
- `string.h` 라이브러리 구문: https://www.tutorialspoint.com/c_standard_library/string_h.htm