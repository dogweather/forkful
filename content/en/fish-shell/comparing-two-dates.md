---
title:    "Fish Shell recipe: Comparing two dates"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why 
When working with dates, it can be useful to compare them to determine which one is greater or if they are equal. This can be especially helpful in tasks such as sorting data or organizing events in chronological order. With Fish Shell, comparing two dates is a quick and simple process.

## How To
To compare two dates in Fish Shell, we will use the ```fish_date``` command. This command returns the current time and date in the format specified by the user. Here's an example of how it works:

```Fish Shell
fish_date +%s
```

The ```+%s``` option tells the ```fish_date``` command to return the date in seconds since the Unix epoch, which is January 1, 1970, 00:00:00 UTC. This format is useful as it allows for easy comparison of dates. 

Now, let's say we want to compare two dates to see which one is greater. We can do this by using the ```-lt``` (less than) and ```-gt``` (greater than) options in a conditional statement. Here's an example:

```Fish Shell
if [ (fish_date -lt "2021-01-01") and (fish_date -gt "2020-01-01") ]
    echo "2020 is greater than 2021"
else
    echo "2021 is greater than 2020"
end
```

In this example, we are comparing two dates, one being the current date (2021-02-12) and the other being a past date (2020-01-01). Since 2021 is greater than 2020, the output will be "2021 is greater than 2020". 

## Deep Dive
It's important to note that when comparing dates, Fish Shell uses the YYYY-MM-DD format. This means that when using the ```-lt``` and ```-gt``` options, the dates must be entered in this format for the comparison to work correctly. Additionally, for more complex comparisons, Fish Shell also has the ```-eq``` (equal to) and ```-ne``` (not equal to) options.

## See Also
For more information on the ```fish_date``` command and its options, check out the official Fish Shell documentation: 
- https://fishshell.com/docs/current/cmds/date.html 

For additional tips and tricks on using Fish Shell, take a look at these helpful resources:
- https://fishshell.com/docs/current/tutorial.html 
- https://fishshell.com/docs/current/index.html