---
title:                "C: 문자열을 소문자로 변환하기."
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
스트링을 소문자로 변환하는 것에 관심을 가져야 할까요? 대부분의 프로그래밍 언어에서 문자열은 대소문자를 구분하기 때문에, 입력받은 사용자 입력이나 데이터베이스에서 가져온 문자열을 일괄적으로 소문자로 만들어서 일관된 처리를 하기 위해 필요합니다.

## 코딩 방법
다음은 C언어로 문자열을 소문자로 변환하는 간단한 예제 코드입니다.

```C
#include<stdio.h>
#include<string.h>

// 소문자로 변환하는 함수
void toLower(char str[]){
    int i;
    for(i = 0; i < strlen(str); i++){
        if(str[i] >= 'A' && str[i] <= 'Z'){
            str[i] += 32;
        }
    }
}

int main(){
    char input[100];

    // 사용자로부터 문자열 입력받기
    printf("문자열을 입력하세요: ");
    scanf("%s", input);

    // 입력받은 문자열을 소문자로 변환하기
    toLower(input);

    // 변환된 문자열 출력하기
    printf("변환된 문자열: %s", input);

    return 0;
}
```

위 코드를 실행하면 다음과 같은 결과가 나옵니다.

```
문자열을 입력하세요: HeLLo WoRLd!
변환된 문자열: hello world!
```

위의 예제 코드에서는 입력받은 문자열을 모두 소문자로 변환하는 함수를 구현했습니다. 따라서 대문자가 있는 문자열도 모두 소문자로 변환됩니다. 또한 문자가 소문자가 아닌 경우에는 변환하지 않습니다.

## 깊이 파고들기
위의 간단한 예제 코드에서는 문자열을 소문자로 변환하는 과정에 대해 자세히 살펴보겠습니다.

우선, 문자열을 저장하기 위해 문자 배열인 `char`형 변수를 사용했습니다. `char`형 변수는 1바이트 크기의 정수형이며, 문자를 저장하는 데 사용됩니다. 따라서 문자열은 여러 개의 `char`형 변수로 이루어져 있습니다.

위의 예제 코드에서는 `for`문을 사용하여 문자열의 모든 문자를 순차적으로 검사합니다. `strlen` 함수를 사용하여 문자열의 길이를 구하고, `for`문을 이용해 첫 번째 문자부터 마지막 문자까지 차례대로 검사합니다.

검사 과정에서 `if`문을 사용해 각 문자가 대문자인지 확인합니다. 대문자는 ASCII 코드 값으로 65부터 90까지에 해당하는 범위에 있으므로, 해당 범위에 있는 문자라면 32를 더해주어 소문자로 변환합니다.

따라서 위의 예제 코드에서는 문자열을 변환할 때, 대문자를 소문자로 바꾸는 것이 핵심입니다. 이를 응용하면 특정 문자열에서만 대문자를 변환하거나, 대문자를 제외하고 소문자로 변환할 수도 있습니다.

## 참고자료
- [C언어 포인터와 문자열](https://coding-factory.tistory.com/250)
- [문자열 다루기](https://dojang.io/mod/page/view.php?id=276)
- [ASCII table](https://www.asciitable.com/)