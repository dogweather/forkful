---
title:    "Bash recipe: Comparing two dates"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

If you're a programmer, you know that dealing with dates can be a tricky task. Often times, we need to compare two dates in our code, such as checking if a certain date is before or after another date. This may seem simple, but it can get complicated quickly depending on different formats and time zones. That's why it's important to have a solid understanding of how to compare dates in Bash.

## How To

To start off, let's take a look at how we can compare two dates in Bash using the `date` command. Here's an example:

```
#!/bin/bash

date1="2020-01-01"
date2="2020-01-02"

if [[ "$date1" < "$date2" ]]; then
  echo "$date1 is before $date2"
elif [[ "$date1" > "$date2" ]]; then
  echo "$date2 is before $date1"
else
  echo "The dates are the same"
fi
```

In this example, we have two dates assigned to variables `date1` and `date2`. We then use the `if` statement to compare the two dates. The `[[ $date1 < $date2 ]]` syntax checks if `date1` is before `date2`. Similarly, `[[ $date1 > $date2 ]]` checks if `date1` is after `date2`. The `else` statement will be executed if the dates are the same.

We can also use the `-lt` and `-gt` operators to compare dates. Here's an example:

```
if [[ "$date1" -lt "$date2" ]]; then
  echo "$date1 is before $date2"
elif [[ "$date1" -gt "$date2" ]]; then
  echo "$date2 is before $date1"
else
  echo "The dates are the same"
fi
```

Another useful tool for comparing dates is the `date -d` command. This allows us to specify a date using a different format and compare it to another date. Here's an example:

```
date1="2020-01-01"
date2=$(date -d "1 day" +"%Y-%m-%d")

if [[ "$date1" == "$date2" ]]; then
  echo "The dates are the same"
else
  echo "$date1 is not the same as $date2"
fi
```

In this example, we use `date -d` to add 1 day to our `date1` variable and format it to match `date2`. Then, we simply check if the two dates are the same.

## Deep Dive

As we mentioned earlier, comparing dates becomes more complex when considering different time zones and date formats. Thankfully, Bash provides us with some tools to handle these situations.

One of these tools is the `TZ` variable, which allows us to specify a different time zone when comparing dates. Here's an example:

```
TZ='America/New_York' date1=$(date +"%Y-%m-%d")
TZ='Europe/London' date2=$(date +"%Y-%m-%d")

if [[ "$date1" < "$date2" ]]; then
  echo "$date1 is before $date2"
elif [[ "$date1" > "$date2" ]]; then
  echo "$date2 is before $date1"
else
  echo "The dates are the same"
fi
```

In this example, we use the `TZ` variable to set `date1` in New York and `date2` in London. We then use our previous methods to compare the two dates.

## See Also

- Bash scripting basics: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
- Date comparison in Python blog post: https://realpython.com/python-datetime/
- Bash 'date' command documentation: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html