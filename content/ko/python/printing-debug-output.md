---
title:                "Python: 디버그 출력 출력"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

프로그래머라면 항상 디버그를 해야합니다. 그리고 디버깅 과정에서는 많은 메시지들이 출력됩니다. 이 메시지들은 제작된 소스코드를 이해하는데 큰 도움을 주며, 가끔은 오류로 인해 잘못 출력되는 경우도 있습니다. 디버그 출력은 코드를 디버그하는데 매우 유용하며, 보다 효율적인 디버깅 과정을 위해 반드시 익히고 사용해야 합니다.

## 어떻게 해야 할까요?

디버그 출력을 하기 위해서는 `print()` 함수를 사용하면 됩니다. 이 함수는 매우 간단하게 사용할 수 있으며, 원하는 정보를 출력할 수 있습니다.

```Python
# 디버그를 위한 print 함수 호출
print("디버그 메시지 출력")
```

출력 결과는 다음과 같이 나타납니다.

```Python
디버그 메시지 출력
```

이렇게 간단한 코드를 작성하면서 디버그 출력을 시작할 수 있습니다. 하지만, 더 많은 정보나 변수 값을 출력하고 싶다면 여러 가지 방법을 사용할 수 있습니다.

```Python
# 변수의 값과 함께 디버그 출력하기
num1 = 10
num2 = 20
print("num1의 값: ", num1)
print("num2의 값: ", num2)
```

출력 결과는 다음과 같이 나타납니다.

```Python
num1의 값: 10
num2의 값: 20
```

또는 `format()` 함수를 사용하여 출력 결과를 더 깔끔하게 만들 수 있습니다.

```Python
# 변수의 값과 함께 디버그 출력하기 (format 함수 사용)
num1 = 10
num2 = 20
print("num1의 값: {}, num2의 값: {}".format(num1, num2))
```

출력 결과는 위의 예제와 동일하지만, 코드가 간결해진 것을 확인할 수 있습니다. 이와 같은 방법 외에도 `f-string`이라는 새로운 방법을 사용할 수도 있습니다.

```Python
# 변수의 값과 함께 디버그 출력하기 (f-string 사용)
num1 = 10
num2 = 20
print(f"num1의 값: {num1}, num2의 값: {num2}")
```

출력 결과는 위의 예제와 동일합니다.

## 깊게 들어가기

디버그 출력은 가끔 상황에 따라 정확한 정보를 제공하지 못할 수도 있습니다. 그래서 `assert`문을 사용하여 특정 조건이 만족될 때만 출력되도록 할 수 있습니다.

```Python
# assert 문을 사용한 디버그 출력
num = 5
assert num > 10, "num의 값은 10보다 커야 합니다."
```

위의 코드에서는 `num`이 10보다 크지 않으므로 `AssertionError`가 발생합니다. 이를 해결하기 위해서는 `assert`문 뒤의 조건을 만족하도록 `num`의 값을 수정하면 됩니다.

또한, `logging` 모듈을 사용하면 디버그 출력을 좀 더 효율적으로 관리할 수 있습니다. 해당 모듈은 출력 정보를 로그 파일로 남기고, 필요한 경우에만 적절한 레벨로 로그를 출력합니다.

## 참고 자료

- "[Python 디버깅 출력 참고 문서](https://wikidocs.net/7138)"
- "[Python assert 문 참고 문서](https://wikidocs.net/21051)"
- "[Python logging 모듈 참고 문서](