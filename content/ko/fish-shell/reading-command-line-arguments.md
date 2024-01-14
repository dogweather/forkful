---
title:    "Fish Shell: 컴퓨터 프로그래밍에서의 명령 줄 인수 읽기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

Fish Shell 프로그래밍에 대한 이야기를 나누려는 사람들을 위해 Fisher는 콘솔 인자를 읽는 방법을 이해하는 것이 중요합니다. 이것은 효율적인 쉘 스크립트 작성의 핵심 요소입니다.

## 어떻게

```Fish Shell```이 포함 된 코드 블록에서 다양한 예시와 샘플 출력을 사용하여 흥미롭고 유익한 콘솔 인자 읽기 방법을 배워 보세요.

하나의 콘솔 인자 읽기 예시는 다음과 같습니다:
```
argv[2]
```
위의 예시에서, ```argv```는 명령어 라인 인자(예: ```ls```, ```mkdir```, ```cp```)를 나타냅니다. ```argv```의 ```2```는 인자의 인덱스를 나타냅니다.

이 코드는 사용자가 쉘에 ```ls -l```을 입력하면 ```argv[2]```의 값으로 ```-l```이 출력됩니다.

## 깊이 파고들기

콘솔 인자를 읽는 것은 프로그래밍의 깊게 파고드는 일입니다. 쉘 스크립트를 이용하여 복잡한 작업을 수행할 수 있으며, 콘솔 인자를 읽는 것은 작업의 정확성과 효율성에 매우 중요한 역할을 합니다.

여러분은 ```argv```를 포함하여 다양한 명령어에 대한 콘솔 인자 라이브러리를 살펴볼 수 있습니다. 이것들은 여러분이 쉘 스크립트를 더 효율적으로 작성할 수 있게 도와주는 하나의 도구일 뿐입니다.

## 또 다른 참고자료

- [Fish Shell 공식 사이트] (https://fishshell.com/)
- [Fish Shell 세부 정보 및 예제] (https://www.cyberciti.biz/faq/bash-command-line-arguments/)

*이것은 Fisher의 콘솔 인자 읽기에 대한 간결한 개요입니다. 더 많은 정보를 얻으려면 정식 공식 문서를 확인해 보세요.*