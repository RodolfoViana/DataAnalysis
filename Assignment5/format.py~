import codecs
import unicodedata


def remove_accents(input_str):
    nkfd_form = unicodedata.normalize('NFKD', input_str)
    only_ascii = nkfd_form.encode('ASCII', 'ignore')
    return only_ascii

programacao = codecs.open('test_second_round_kaggle.csv',encoding='latin1')
f = open('test_second_round_kaggle_sem_acento.csv', 'w')



for lines in programacao:
	f.write(remove_accents(lines.strip())+"\n")
	print remove_accents(lines.strip())


