import csv
import random

words = []
#Windows:
#csv_data_path = "C:\python\\venv\latincsv.csv"
csv_data_path = "latin_csv.csv"

csv_reader = csv.reader(open(csv_data_path))
latin_data = [line for line in csv_reader]


def generate_list(length):

    return [latin_data[i] for i in random.sample(range(0, len(latin_data) - 1), length)]


def relevance(a, b):

    tags_shared = len([value for value in a if value in b])

    total_tags = len(b) - 1  # div by 0 if a word has no tags
    return tags_shared/total_tags


def r(n):

    if(n == 0):
        return 1
    else:
        numerator = 0
        denominator = 0  # div by 0 if n = 0, which should be caught above

        for index in range(0, n):
            denominator += r(index)
            numerator += (relevance(words[index], words[n]) * r(index))

        return numerator/denominator


relevance_value = 0
threshold = .8  # the minimum relevance threshold, 0 <= x <= 1

while relevance_value < threshold:
    words = generate_list(5)
    relevance_value = r(len(words) - 1)

print(relevance_value, ":", words, "\n")
for i in range(0, 5):
    print(words[i][0] + ";", "Relevance:", r(i))
