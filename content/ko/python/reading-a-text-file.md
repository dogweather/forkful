---
title:                "Python: 텍스트 파일 읽기"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜
텍스트 파일을 읽는 방법에 대해 배우는 것은 프로그래밍의 기본 기술 중 하나입니다. 이 작업을 숙련하게 할 수 있으면 더 복잡한 작업을 수행하는 데 필요한 기초를 다질 수 있고, 코드를 이해하고 수정하는 데 있어서도 중요한 기반을 마련할 수 있습니다.

## 어떻게 하나요?
우리가 흔히 쓰는 텍스트 파일에는 여러 가지 형식이 있을 수 있습니다. 여기서는 가장 기본적인 형태인 .txt 파일을 읽는 방법을 살펴보겠습니다. 우선, 파일을 읽을 때 사용할 수 있는 개별 명령어를 배우겠습니다.

```python
# 파일 열기
file = open('example.txt', 'r')

# 파일 내용 읽기
contents = file.read()
print(contents)

# 파일 닫기
file.close()
```

다음은 실제로 파일 내용을 읽을 수 있는 방법입니다. 위 코드를 입력해보고, 다음과 같은 텍스트 파일이 있는지 확인해보세요.

**example.txt** 파일 내용:
```
Hello, world!
This is a sample text file.
```
위 코드를 이용하면 텍스트 파일의 내용을 출력할 수 있습니다. 텍스트 파일이 아니라 다른 형식의 파일을 읽는 방법은 다소 다를 수 있으므로, 해당 파일 형식에 맞는 방법을 찾아보시기 바랍니다.

## 깊게 들어가보기
텍스트 파일을 읽는 방법은 간단해 보이지만, 실제로는 파일을 읽는 과정에서 여러 가지 문제가 발생할 수 있습니다. 예를 들어, 파일이 너무 크면 메모리 부족 오류가 발생할 수 있습니다. 이런 경우에는 파일을 잘게 나누어 처리하는 방법이 필요합니다.

또는 파일의 인코딩 형식이 다를 경우 문제가 발생할 수도 있습니다. 이런 경우에는 파일을 열 때 인코딩 형식을 지정해주는 것이 중요합니다.

아직은 모든 상황을 대처할 수는 없지만, 위에서 배운 기본적인 방법과 여러 가지 팁을 적용하여 파일을 잘 읽는 마스터가 되어보세요!

## 참고하기
- [파이썬 파일 관리 기능](https://dojang.io/mod/page/view.php?id=2208)
- [파일 열기, 쓰기, 닫기](http://pythonstudy.xyz/python/article/201-%ED%8C%8C%EC%9D%BC-%EC%97%B4%EA%B8%B0,-%EC%93%B0%EA%B8%B0,-%EB%8B%AB%EA%B8%B0)