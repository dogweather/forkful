---
title:    "Python: 디버그 출력 찍기"
keywords: ["Python"]
---

{{< edit_this_page >}}

## 왜 디버그 출력하는 걸 할까요?

디버그 출력을 사용함으로써 우리는 우리가 작성한 코드를 검사하고 버그를 찾아낼 수 있습니다. 이는 더 나은 코드를 작성하고 프로그래밍 기술을 향상시키는 데 도움이 됩니다.

## 디버그 출력하는 방법

디버그 출력은 ```print()``` 함수를 사용하여 손쉽게 할 수 있습니다. 우리가 간단한 예제로 시작해보겠습니다.

```Python
name = "홍길동"
age = 28
print("제 이름은 " + name + "이고 나이는 " + str(age) + "살입니다.")
```

위의 코드를 실행하면 다음과 같은 출력 결과가 나올 것입니다.

```
제 이름은 홍길동이고 나이는 28살입니다.
```

또한 중간에 변수의 값을 확인하고 싶다면 ```print()``` 함수를 사용하여 출력할 수 있습니다.

```Python
num1 = 10
num2 = 5
total = num1 + num2
print("num1 과 num2 의 합은 " + str(total) + "입니다.")
print("num1 의 값은 " + str(num1) + "입니다.")
print("num2 의 값은 " + str(num2) + "입니다.")
```

위의 코드를 실행하면 다음과 같은 출력 결과가 나올 것입니다.

```
num1 과 num2 의 합은 15입니다.
num1 의 값은 10입니다.
num2 의 값은 5입니다.
```

## 디버그 출력의 깊은 이해

디버그 출력을 사용해서 코드를 검사하는 것은 매우 중요합니다. 그러나 디버그 출력이 너무 많으면 코드의 가독성이 떨어지고 코드가 복잡해질 수 있습니다. 따라서 디버그 출력을 사용할 때는 신중하게 선택하여 사용하는 것이 좋습니다.

## 같이 보기

- [Python 기초: 변수 사용하기](https://www.opentutorials.org/course/1750/9615)
- [print() 함수 설명서](https://docs.python.org/3/library/functions.html#print)
- [The Ultimate Guide to Debugging in Python](https://realpython.com/python-debugging-pdb/)