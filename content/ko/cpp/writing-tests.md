---
title:    "C++: 프로그래밍에서의 테스트 작성"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/writing-tests.md"
---

{{< edit_this_page >}}

# 왜 컴퓨터 프로그래밍에서 테스트를 써야할까?

컴퓨터 프로그래밍은 복잡한 과정입니다. 많은 코드와 다양한 기능들이 포함되어 있으며, 이 모든 것을 완벽하게 작동시키기 위해서는 수많은 노력과 시간이 필요합니다. 이런 복잡성 속에서 오류가 발생할 수 있으며, 이는 컴퓨터 프로그램의 기능을 저해할 수 있습니다. 따라서, 테스트를 적용하여 오류를 최소화하고 완벽한 프로그램을 개발하는 데 도움을 줄 수 있습니다.

## 어떻게 테스트 코드를 작성할 수 있을까?

우리는 C++ 언어를 사용하여 간단한 예제를 보여줄 것입니다. 먼저, 테스트를 위해 다음과 같은 코드를 작성합니다.

```C++
#include <iostream>
using namespace std;

// 두 수를 더하는 함수
int add(int a, int b){
    return a + b;
}

// 두 수를 곱하는 함수
int multiply(int a, int b){
    return a * b;
}
```

위 코드에서는 `add` 함수와 `multiply` 함수를 가지고 있습니다. 이제 `main` 함수에서 이 두 함수를 테스트해보겠습니다.

```C++
int main(){
    // add 함수의 테스트
    int result_add = add(3, 4);
    if(result_add == 7){
        // 함수가 정확하게 작동하므로 'Success'를 출력
        cout << "Success" << endl;
    } else{
        // 함수의 결과가 예상한 값과 다르므로 'Failure'를 출력
        cout << "Failure" << endl;
    }

    // multiply 함수의 테스트
    int result_multiply = multiply(2, 5);
    if(result_multiply == 10){
        // 함수가 정확하게 작동하므로 'Success'를 출력
        cout << "Success" << endl;
    } else{
        // 함수의 결과가 예상한 값과 다르므로 'Failure'를 출력
        cout << "Failure" << endl;
    }

    return 0;
}
```

위 코드에서는 각 함수의 결과를 확인하고, 예상한 값과 일치하는지 여부에 따라 `Success` 또는 `Failure`를 출력합니다. 이렇게 테스트하면 코드를 수정할 때 어떤 함수에서 오류가 발생했는지 쉽게 파악할 수 있습니다.

## 테스트 코드를 작성하는 더 깊은 고민

우리는 단순히 예상한 값과 결과 값을 비교하여 테스트하였지만, 테스트 코드를 작성할 때 더 깊이 생각해야 할 사항들이 있습니다. 예를 들어, 함수에 입력할 수 있는 최대 값, 최소 값, 그리고 오류 처리 등을 고려해야 합니다. 또한, 여러 가지 상황에서 함수가 예상한 대로 작동하는지 테스트해보아야 합니다. 이런 깊은 고민을 통해 더 효율적인 테스트 코드를 작성할 수 있습니다.

# 참고 자료들

- [The Art of Writing Software Test, Medium](https://medium.com/@jonhoo/the-art-of-writing-software-tests-6abebca9a45a)
- [Effective Testing with C++, Pluralsight](https://www.pluralsight.com/courses/effective-testing-cplusplus)
- [Google Test, Google Open Source](https://github.com/google/googletest)