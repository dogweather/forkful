---
title:                "Bash: 미래나 과거의 날짜를 계산하기"
simple_title:         "미래나 과거의 날짜를 계산하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

#왜
어떤 이유에서 어떤 사람이 과거나 미래 날짜를 계산하는데 참여할지에 대해서는 매우 궁금할 수 있습니다. 이러한 계산은 어떤 상황에서 유용할까요? 아무래도 날짜를 조정할 필요가 있을 때마다 참여할 수 있어서 저는 이 주제가 흥미로워서 이번에는 이에 대해 알려드리려고 합니다.

#어떻게
이러한 계산은 Bash script를 이용하여 진행될 수 있습니다. 먼저 그것이 무엇이고, 그것이 어떻게 사용되어지는지에 대해서 알아보도록 하겠습니다. 먼저 Bash script를 찾아서 그것을 우리가 원하는 방식으로 실행할 수 있도록 하는 것으로 시작해보겠습니다. 다음으로 그것이 어떻게 작동하는지에 대해서 알아보겠습니다.

```Bash
#!/bin/bash

# 오늘의 날짜를 가져오기
today=$(date +%Y-%m-%d)

# 5일 후의 날짜를 계산하여 출력
future_date=$(date -d "+5 days" +%Y-%m-%d)
echo "5일 후의 날짜: $future_date"

# 10년 전의 날짜를 계산하여 출력
past_date=$(date -d "10 years ago" +%Y-%m-%d)
echo "10년 전의 날짜: $past_date"
```

위의 Bash script는 현재 날짜를 가져온 다음, "date" 명령어를 사용하여 5일 후와 10년 전의 날짜를 계산하고 출력하는 예제입니다. 우리가 원하는 범위의 날짜를 자유롭게 입력하여 계산할 수 있습니다.

#깊게 파고들기
때때로 우리는 과거 또는 미래 날짜를 특정한 형식으로 계산해야 할 수 있습니다. 이는 예를 들어, 일주일 뒤의 월요일의 날짜를 알고 싶거나, 한 달 전의 마지막 날짜를 알고 싶을 때 등 다양한 상황에서 사용될 수 있습니다.

Bash script를 사용하여 미래 또는 과거 날짜를 계산하는 것은 매우 쉽습니다. 우리는 "date" 명령어의 다양한 옵션을 이용하여 원하는 방식대로 날짜를 계산할 수 있습니다. 실제로, 이 모든 옵션들을 알아보는 것은 제가 이 블로그 포스트에서 다루기에는 너무 많은 주제가 될 것입니다.

관심있는 분들은 "man date" 명령어를 참조하거나 다른 자료들을 검색하여 더 많은 정보를 얻을 수 있습니다.

#See Also
[Bash에서 날짜 및 시간 계산하기](https://www.howtogeek.com/463614/how-to-add-and-subtract-dates-in-bash/)
[Bash 사용자를 위한 효과적인 날짜 형식 지정 방법](https://www.howtogeek.com/558702/how-to-format-date-and-time-in-the-linux-terminal-and-use-it-in-bash-scripts/)
[Bash 공식 문서](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)