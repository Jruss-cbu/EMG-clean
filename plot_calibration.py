import matplotlib.pyplot as plt
import numpy as np
import os

def plot_last_emg_cal(path, index):
    file_path = os.path.join(path,f'Env_EMG_{index}.txt')
    ext_values = []
    flx_values = []
    bic_values = []
    tri_values = []

    with open(file_path, 'r') as file:
        for line in file:
            # Strip any extra whitespace and split by space or any whitespace
            values = line.strip().split()
            if len(values) != 4:
                print(f"Unexpected number of values in line: '{line.strip()}', expected 2.")
                continue

            try:
                first_value = float(values[0])
                second_value = float(values[1])
                third_value = float(values[2])
                fourth_value = float(values[3])
            except ValueError as e:
                print(f"Skipping line due to value error: {e}")
                continue

            flx_values.append(first_value)
            ext_values.append(second_value)
            bic_values.append(third_value)
            tri_values.append(fourth_value)

        time = np.linspace(0, 75, num=len(ext_values), endpoint=False)

        contraction_values = [10, 20, 30, 40, 50, 60, 70]
        
        plt.figure(figsize=(10, 6))
        plt.subplot(4, 1, 1)
        plt.plot(time, flx_values)
        plt.title('Flexors EMG Signal')
        plt.xlabel('Time(s)')
        plt.ylabel('Amplitude')

        for flag_value in contraction_values:
            plt.axvline(x=flag_value, color='r', linestyle='--', linewidth=1)

        plt.subplot(4, 1, 2)
        plt.plot(time, ext_values)
        plt.title('Extensors EMG Signal')
        plt.xlabel('Time (s)')
        plt.ylabel('Amplitude')

        for flag_value in contraction_values:
            plt.axvline(x=flag_value, color='r', linestyle='--', linewidth=1)

        plt.subplot(4, 1, 3)
        plt.plot(time, bic_values)
        plt.title('Biceps EMG Signal')
        plt.xlabel('Time (s)')
        plt.ylabel('Amplitude')

        for flag_value in contraction_values:
            plt.axvline(x=flag_value, color='r', linestyle='--', linewidth=1)
        
        plt.subplot(4, 1, 4)
        plt.plot(time, tri_values)
        plt.title('Triceps EMG Signal')
        plt.xlabel('Time (s)')
        plt.ylabel('Amplitude')

        for flag_value in contraction_values:
            plt.axvline(x=flag_value, color='r', linestyle='--', linewidth=1)

        plt.tight_layout()
        plt.show()

if __name__ == '__main__':
    #path = '/Users/Julien/Desktop/AT Data/data_pt_17/EMG_calibration_pt_17/EMG_Calibration_day_4'
    path ='/Volumes/LaCie/ResearchProjects/Null_Space_EMG/PT_17/PT_17_Day4/EMG_Calibration_day_4/'
    index = 1
    plot_last_emg_cal(path, index)